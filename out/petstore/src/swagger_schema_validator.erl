%% -*- mode: erlang -*-
-module(swagger_schema_validator).

-behaviour(jesse_schema_validator).

%% API
-export([validate/5]).

%% Behaviour callbacks
-export([init_state/1]).
-export([check_value/3]).

-define(TYPE,                 <<"type">>).                 %mild
-define(PROPERTIES,           <<"properties">>).           %mild
-define(PATTERNPROPERTIES,    <<"patternProperties">>).    %mild
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>). %mild
-define(ITEMS,                <<"items">>).                %mild
-define(ADDITIONALITEMS,      <<"additionalItems">>).      %mild
-define(REQUIRED,             <<"required">>).             %mild
-define(DEPENDENCIES,         <<"dependencies">>).         %mild
-define(MINIMUM,              <<"minimum">>).              %strict
-define(MAXIMUM,              <<"maximum">>).              %strict
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).     %strict
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).     %strict
-define(MINITEMS,             <<"minItems">>).             %mild
-define(MAXITEMS,             <<"maxItems">>).             %mild
-define(UNIQUEITEMS,          <<"uniqueItems">>).          %strict
-define(PATTERN,              <<"pattern">>).              %strict
-define(MINLENGTH,            <<"minLength">>).            %strict
-define(MAXLENGTH,            <<"maxLength">>).            %strict
-define(ENUM,                 <<"enum">>).                 %strict
-define(ANYOF,                <<"anyOf">>).                %mild
-define(ONEOF,                <<"oneOf">>).                %mild
-define(NOT,                  <<"not">>).                  %mild
-define(MULTIPLEOF,           <<"multipleOf">>).           %strict
-define(MAXPROPERTIES,        <<"maxProperties">>).        %mild
-define(MINPROPERTIES,        <<"minProperties">>).        %mild

-define(DISCRIMINATOR, <<"discriminator">>).               %mild
-define(ALLOF,         <<"allOf">>).                       %mild
-define(FORMAT,        <<"format">>).                      %strict
-define(REF,           <<"$ref">>).                        %mild
-define(DEFINITIONS,   "definitions").
-define(NOT_FOUND,     not_found).
-define(READ_ONLY,     <<"readOnly">>).                    %mild

-define(DISCR_ERROR(Error), {discriminator_not_valid, Error}).
-define(READ_ONLY_ERROR, read_only_property_in_request).

-type validation_keyword() :: binary().
-type validation_keyword_list() :: list(validation_keyword()).
-type validation_ruleset() :: none | mild | strict | validation_keyword_list().

-type msg_type() :: swagger_validation:msg_type().
-type schema_validation_opts() :: #{msg_type() => validation_ruleset()}.
-type validation_opts() :: swagger_validation:validation_opts().

-type schema_rule() :: {binary(), jesse:json_term()}.
-type schema_path() :: [binary() | non_neg_integer()].

-export_type([schema_validation_opts/0]).
-export_type([schema_rule/0]).
-export_type([schema_path/0]).

-type context() ::
    #{
        operation_id := swagger:operation_id(),
        msg_type     := msg_type(),
        definition_name     := swagger:param_name(),
        current_path => schema_path()
    }.

-export_type([context/0]).

-type state() ::
    #{
        refs => [],
        validation_meta => context(),
        validation_keywords => validation_keyword_list(),
        custom_validator => module()
    }.

%%
-spec init_state(Opts :: jesse_state:validator_opts()) -> state().

init_state(Opts) ->
    Opts#{refs => []}.

-spec validate(
    OperationID :: swagger:operation_id(),
    Value      :: swagger:value(),
    DefinitionName    :: swagger:param_name(),
    MsgType    :: msg_type(),
    ValidationOpts :: validation_opts()
) ->
    ok | {error, Error :: swagger:error_reason()}.
validate(OperationID, Value, DefinitionName, MsgType, ValidationOpts) ->
    validate(OperationID, Value, DefinitionName, MsgType, swagger_schema:get(), ValidationOpts).

validate(OperationID, Value, DefinitionName, MsgType, Schema, ValidationOpts) ->
    ValidationKeywords = get_validation_keywords(MsgType, ValidationOpts),
    CustomValidator    = maps:get(custom_validator, ValidationOpts, undefined),
    Options = [
        {validator_opts, #{
            validation_meta => make_context(OperationID, DefinitionName, MsgType),
            validation_keywords => ValidationKeywords,
            custom_validator => CustomValidator
        }} | options()
    ],
    case jesse:validate_definition(swagger_utils:to_list(DefinitionName), Schema, Value, Options) of
        {ok, _} ->
            ok;
        {error, [Error]} ->
            {error, map_error_reason(Error)}
    end.

make_context(OperationID, DefinitionName, MsgType) ->
    #{
        operation_id => OperationID,
        definition_name => DefinitionName,
        msg_type => MsgType
    }.

get_validation_keywords(MsgType, ValidationOpts) ->
    SchemaOpts = maps:get(schema, ValidationOpts, #{}),
    Ruleset = maps:get(MsgType, SchemaOpts, strict),
    expand_ruleset(Ruleset).

expand_ruleset(Keywords) when is_list(Keywords) ->
    Keywords;
expand_ruleset(none) ->
    [];
expand_ruleset(mild) ->
    [
        ?TYPE, ?PROPERTIES, ?PATTERNPROPERTIES, ?ADDITIONALPROPERTIES, ?ITEMS,
        ?ADDITIONALITEMS, ?REQUIRED, ?DEPENDENCIES, ?ALLOF, ?ANYOF, ?ONEOF, ?NOT,
        ?REF, ?DISCRIMINATOR, ?READ_ONLY
    ];
expand_ruleset(strict) ->
    expand_ruleset(mild) ++ [
        ?MINIMUM, ?MAXIMUM, ?EXCLUSIVEMINIMUM, ?EXCLUSIVEMAXIMUM, ?UNIQUEITEMS,
        ?PATTERN, ?MINLENGTH, ?MAXLENGTH, ?MINITEMS, ?MAXITEMS, ?MAXPROPERTIES,
        ?MINPROPERTIES, ?ENUM, ?FORMAT, ?MULTIPLEOF
    ].

-spec check_value(
    Value :: any(),
    Attr  :: {binary(), jesse:json_term()},
    State :: jesse_state:state()
) ->
    State :: jesse_state:state() |
    no_return().

check_value(Value, Attr, State) ->
    try
        maybe_use_custom_validator(Value, Attr, State)
    catch
        throw:Errors ->
            case handle_check_value_errors(Errors, Attr, Value, State) of
                [] -> State;
                Unhandled -> throw(Unhandled)
            end
    end.

maybe_use_custom_validator(Value, Attr, State) ->
    case swagger_custom_validator:validate_schema(Attr, Value, State) of
        pass  ->
            do_check_value(Value, Attr, State);
        State ->
            State
    end.

do_check_value(Value, {?DISCRIMINATOR, DiscrField}, State) ->
    case jesse_lib:is_json_object(Value) of
        true  -> validate_discriminator(Value, DiscrField, State);
        false -> State
    end;
do_check_value(Value, {?ALLOF, Schemas}, State) ->
    check_all_of(Value, Schemas, State);
do_check_value(Value, Attr = {?REF, Ref}, State) ->
    case is_recursive_ref(Ref, State) of
        true -> State;
        false -> validate_ref(Value, Attr, State)
    end;
do_check_value(Value, {?READ_ONLY, ReadOnly}, State) ->
    validate_read_only(Value, ReadOnly, State);
do_check_value(Value, {?FORMAT, Type}, State) when
    Type =:= <<"float">> orelse
    Type =:= <<"byte">>  orelse
    Type =:= <<"binary">>
->
    case swagger_common_validator:validate({type, erlang:binary_to_atom(Type, utf8)}, Value) of
        error ->
            jesse_error:handle_data_invalid(wrong_format, Value, State);
        _ ->
            State
    end;
do_check_value(Value, {?FORMAT, Format}, State) when
    Format =:= <<"int32">>      orelse
    Format =:= <<"int64">>      orelse
    Format =:= <<"date">>       orelse
    Format =:= <<"date-time">>  orelse
    Format =:= <<"ip-address">> orelse
    Format =:= <<"email">>      orelse
    Format =:= <<"http-url">>   orelse
    Format =:= <<"uri">>
->
    case swagger_common_validator:validate({format, erlang:binary_to_atom(Format, utf8)}, Value) of
        error ->
            jesse_error:handle_data_invalid(wrong_format, Value, State);
        _ ->
            State
    end;
do_check_value(Value, Attr, State) ->
    jesse_validator_draft4:check_value(Value, Attr, State).

handle_check_value_errors(Errors, Attr, Value, State) ->
    #{validation_keywords := ValidationKeywords} = jesse_state:get_validator_state(State),
    lists:filter(
        fun(Error) ->
            is_error_unignorable(Error, Attr, Value, ValidationKeywords)
        end,
        Errors
    ).

is_error_unignorable(Error, Attr = {Keyword, _}, Value, ValidationKeywords) ->
    case is_keyword_validated(Keyword, ValidationKeywords) of
        true -> true;
        false -> is_error_reason_unsupported(Error, Value, Attr)
    end.

is_keyword_validated(Keyword, Keywords) ->
    lists:member(Keyword, Keywords).

-spec is_error_reason_unsupported(Error, Value, Attr) -> boolean() when
    Error :: jesse_error:error_reason(),
    Value :: any(),
    Attr  :: {binary(), jesse:json_term()}.

is_error_reason_unsupported(Error, Value, Attr) when element(1, Error) =:= data_invalid ->
    _ = log_validation_warning(Error, Value, Attr),
    false;
is_error_reason_unsupported(Error, _Value, _Attr) when element(1, Error) =:= schema_invalid ->
    true.

log_validation_warning(Error, Value, Attr) ->
    Reason = map_error_reason(Error),
    error_logger:warning_msg(
        "Swagger validation failed, but it was ignored: ~p. Expected: ~p. Got: ~p.",
        [Reason, Attr, Value]
    ).

validate_discriminator(Value, DiscrField, State) when is_binary(DiscrField) ->
    case jesse_json_path:value(DiscrField, Value, ?NOT_FOUND) of
        ?NOT_FOUND ->
            State;
        SchemaName ->
            validate_child_schema(Value, SchemaName, State)
    end.

validate_child_schema(Value, SchemaName, State) ->
    Ref    = <<"#/" ?DEFINITIONS "/", SchemaName/binary>>,
    BadRef = swagger_utils:to_list(Ref),
    Schema = make_ref_schema(Ref),
    try jesse_schema_validator:validate_with_state(Schema, Value, State)
    catch
        throw:[{schema_invalid, _Schema, {schema_not_found, BadRef}}] ->
            jesse_error:handle_data_invalid(?DISCR_ERROR(SchemaName), Value, State)
    end.

check_all_of(Value, [_ | _] = Schemas, State) ->
    check_all_of_(Value, Schemas, State);
check_all_of(_Value, _InvalidSchemas, State) ->
    jesse_error:handle_schema_invalid(wrong_all_of_schema_array, State).

check_all_of_(_Value, [], State) ->
    State;
check_all_of_(Value, [Schema | Schemas], State) ->
    check_all_of_(Value, Schemas, validate_schema(Value, Schema, State)).

validate_schema(Value, Schema, State0) ->
    case jesse_lib:is_json_object(Schema) of
        true ->
            State1 = jesse_state:set_current_schema(State0, Schema),
            jesse_schema_validator:validate_with_state(Schema, Value, State1);
        false ->
            jesse_error:handle_schema_invalid(schema_invalid, State0)
    end.

validate_ref(Value, Attr = {?REF, Ref} , State) ->
    Path = jesse_state:get_current_path(State),
    State1 = add_ref_to_state(State, ref_tag(Ref, Path)),
    State2 = jesse_validator_draft4:check_value(Value, Attr, State1),
    remove_last_ref_from_state(State2).

add_ref_to_state(State, Ref) ->
    ValidatorState = jesse_state:get_validator_state(State),
    #{refs := Refs} = ValidatorState,
    jesse_state:set_validator_state(State, ValidatorState#{refs => [Ref | Refs]}).

remove_last_ref_from_state(State) ->
    ValidatorState = jesse_state:get_validator_state(State),
    #{refs := Refs} = ValidatorState,
    case Refs of
        [_ | Rest] ->
            jesse_state:set_validator_state(State, ValidatorState#{refs => Rest});
        [] ->
            State
    end.

is_recursive_ref(Ref, State) ->
    RefTag = ref_tag(Ref, jesse_state:get_current_path(State)),
    #{refs := Refs} = jesse_state:get_validator_state(State),
    lists:member(RefTag, Refs).

make_ref_schema(Ref) ->
    [{?REF, Ref}].

ref_tag(Ref, Path) ->
    {Ref, Path}.

validate_read_only(Value, true, State) ->
    case get_message_type(State) of
        request ->
            jesse_error:handle_data_invalid(?READ_ONLY_ERROR, Value, State);
        response ->
            State
    end.

get_message_type(State) ->
    #{validation_meta := Meta} = jesse_state:get_validator_state(State),
    maps:get(msg_type, Meta).

options() ->
    [{validator, ?MODULE}, {allowed_errors, 0}].

map_error_reason({'data_invalid', _Schema, Error, Data, Path0}) ->
    Path = get_error_path(Path0),
    Description = get_error_description(Error, Data),
    swagger_utils:join(".", [Description, Path]).

get_error_path([]) ->
    <<"">>;
get_error_path(Path0) ->
    Mapper = fun
        (N, Acc) when is_integer(N) ->
            ["[", swagger_utils:to_binary(N), "]" | Acc];
        (X, Acc) ->
            [$., X | Acc]
    end,
    Path2 = case lists:foldr(Mapper, [], Path0) of
        [$.| Path1] -> Path1;
        Path1       -> Path1
    end,
    Path3 = swagger_utils:to_binary(Path2),
    <<" Path to item: ", Path3/binary>>.

get_error_description(all_schemas_not_valid, _Value) ->
    <<"Schema rule \"AllOf\" violated">>;
get_error_description(any_schemas_not_valid, _Value) ->
    <<"Schema rule \"AnyOf\" violated">>;
get_error_description({missing_dependency, Dependency0}, _Value) ->
    Dependency = swagger_utils:to_binary(Dependency0),
    <<"Missing dependency: ", Dependency/binary>>;
get_error_description(missing_required_property, Value) ->
    PropertyName = swagger_utils:to_binary(Value),
    <<"Missing required property: ", PropertyName/binary>>;
get_error_description(no_extra_items_allowed, _Value) ->
    <<"Extra items not allowed">>;
get_error_description(no_extra_properties_allowed, _Value) ->
    <<"Extra properties not allowed">>;
get_error_description(no_match, _Value) ->
    <<"No match to pattern">>;
get_error_description(not_found, _Value) ->
    <<"Not found">>;
get_error_description(not_in_enum, _Value) ->
    <<"Not in enum">>;
get_error_description(not_in_range, _Value) ->
    <<"Not in range">>;
get_error_description(not_multiple_of, _Value) ->
     <<"Schema rule \"MultipleOf\" violated">>;
get_error_description(not_one_schema_valid, _Value) ->
    <<"Schema rule \"OneOf\" violated">>;
get_error_description(not_schema_valid, _Value) ->
    <<"Schema rule \"Not\" violated">>;
get_error_description(too_few_properties, _Value) ->
    <<"Too few properties">>;
get_error_description(too_many_properties, _Value) ->
     <<"Too many properties">>;
get_error_description(wrong_length, _Value) ->
    <<"Wrong length">>;
get_error_description(wrong_size, _Value) ->
    <<"Wrong size">>;
get_error_description(wrong_type, _Value) ->
    <<"Wrong type">>;
get_error_description(wrong_format, _Value) ->
    <<"Wrong format">>;
get_error_description(?DISCR_ERROR(SchemaName), _Value) ->
    <<"Discriminator child schema ", SchemaName/binary, " doesn't exist">>;
get_error_description(?READ_ONLY_ERROR, _Value) ->
    <<"Property that marked as \"readOnly\" must not be sent as part of the request">>.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PET_SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":       {\"type\": \"string\"},
            \"petType\":    {\"type\": \"string\"},
            \"ownerEmail\": {\"type\": \"string\", \"format\": \"email\"},
            \"ownerHomepage\": {\"type\": \"string\", \"format\": \"http-url\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Pig\": {
         \"description\": \"A representation of a pig\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"weight\": {
                 \"type\":        \"integer\",
                 \"description\": \"the weight of the pig\",
                 \"readOnly\":     true
               }
             }
           }
         ]
       }
     }}">>).
-define(PET, 'Pet').

-define(PRED_SCHEMA,
  <<"{\"definitions\": {
       \"Predicate\": {
         \"discriminator\": \"type\",
         \"properties\": {
            \"type\": {\"type\": \"string\"}
         },
         \"required\": [\"type\"]
       },
       \"Constant\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Predicate\"},
           {
             \"properties\": {
               \"type\": {
                 \"type\": \"string\",
                 \"enum\": [\"Constant\"]
               },
               \"value\": {
                 \"type\": \"boolean\"
               }
             }
           }
         ]
       },
       \"Conjunction\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Predicate\"},
           {
             \"properties\": {
               \"type\": {
                 \"type\": \"string\",
                 \"enum\": [\"Conjunction\"]
               },
               \"operands\": {
                 \"type\": \"array\",
                 \"items\": {\"$ref\": \"#/definitions/Predicate\"}
               }
             }
           }
         ]
       }
     }}">>).
-define(PRED, 'Predicate').

test_validate(Value, DefinitionName, BinSchema) ->
    test_validate(Value, DefinitionName, BinSchema, response).

test_validate(Value, DefinitionName, BinSchema, MsgType) ->
    test_validate(Value, DefinitionName, BinSchema, MsgType, #{}).

test_validate(Value, DefinitionName, BinSchema, MsgType, Validation) ->
    Schema    = jsx:decode(BinSchema, [return_maps]),
    JsonValue = jsx:decode(Value, [return_maps]),
    case validate(test, JsonValue, DefinitionName, MsgType, Schema, #{schema => Validation}) of
        ok             -> ok;
        {error, Error} -> Error
    end.

expect(Error, Path) ->
    expect(Error, Path, undefined).

expect(Error, Path, Data) ->
    map_error_reason({data_invalid, undefined, Error, Data, Path}).

%% Test cases
-spec test() -> _.

-spec ok_discr_simple_test() -> _.
ok_discr_simple_test() ->
    Pet = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"adventurous\"
    }">>,
    ?assertEqual(ok, test_validate(Pet, ?PET, ?PET_SCHEMA)).

-spec ok_email_test() -> _.
ok_email_test() ->
    Pet0 = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"adventurous\",
        \"ownerEmail\":   \"me@example.com\"
    }">>,
    ?assertEqual(ok, test_validate(Pet0, ?PET, ?PET_SCHEMA)).

-spec wrong_email_test() -> _.
wrong_email_test() ->
    Pet1 = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"adventurous\",
        \"ownerEmail\":   \"not an email\"
    }">>,
    ?assertEqual(expect(wrong_format, [<<"ownerEmail">>]), test_validate(Pet1, ?PET, ?PET_SCHEMA)).

-spec ok_homepage_test() -> _.
ok_homepage_test() ->
    Pet0 = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"adventurous\",
        \"ownerHomepage\": \"http://localhost/\"
    }">>,
    ?assertEqual(ok, test_validate(Pet0, ?PET, ?PET_SCHEMA)).

-spec wrong_homepage_test() -> _.
wrong_homepage_test() ->
    Pet1 = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"adventurous\",
        \"ownerHomepage\": \"ftp://file@share\"
    }">>,
    ?assertEqual(expect(wrong_format, [<<"ownerHomepage">>]), test_validate(Pet1, ?PET, ?PET_SCHEMA)).

-spec bad_1st_level_discr_simple_test() -> _.
bad_1st_level_discr_simple_test() ->
    Pet = <<"{
        \"name\":         \"Fluffy\",
        \"petType\":      \"Cat\",
        \"huntingSkill\": \"wrong\"
    }">>,
    ?assertEqual(expect(not_in_enum, [<<"huntingSkill">>]), test_validate(Pet, ?PET, ?PET_SCHEMA)).

-spec ok_discr_recursive_definition_test() -> _.
ok_discr_recursive_definition_test() ->
    Predicate = <<"{
        \"type\": \"Conjunction\",
        \"operands\": [
          {\"type\": \"Constant\", \"value\": false},
          {
            \"type\": \"Conjunction\",
            \"operands\": [
              {\"type\": \"Constant\", \"value\": false},
              {\"type\": \"Constant\", \"value\": true}
            ]
          }
        ]
    }">>,
    ?assertEqual(ok, test_validate(Predicate, ?PRED, ?PRED_SCHEMA)).

-spec bad_3d_level_discr_recursive_definition_test() -> _.
bad_3d_level_discr_recursive_definition_test() ->
    Predicate = <<"{
        \"type\": \"Conjunction\",
        \"operands\": [
          {\"type\": \"Constant\", \"value\": false},
          {
            \"type\": \"Conjunction\",
            \"operands\": [
              {\"type\": \"Constant\", \"value\": \"wrong\"},
              {\"type\": \"Constant\", \"value\": true}
            ]
          }
        ]
    }">>,
    ?assertEqual(
        expect(wrong_type, [<<"operands">>, 1, <<"operands">>, 0, <<"value">>]),
        test_validate(Predicate, ?PRED, ?PRED_SCHEMA)
    ).

-spec exceed_int32_swagger_format_test() -> _.
exceed_int32_swagger_format_test() ->
    Pet = <<"{
        \"name\":     \"Rex\",
        \"petType\":  \"Dog\",
        \"packSize\": 2147483650
    }">>,
    ?assertEqual(expect(wrong_format, [<<"packSize">>]), test_validate(Pet, ?PET, ?PET_SCHEMA)).

-spec ok_read_only_request_test() -> _.
ok_read_only_request_test() ->
    Pet = <<"{
        \"name\":     \"Babe\",
        \"petType\":  \"Pig\"
    }">>,
    ?assertEqual(ok, test_validate(Pet, ?PET, ?PET_SCHEMA, request)).

-spec error_read_only_request_test() -> _.
error_read_only_request_test() ->
    Pet = <<"{
        \"name\":     \"Babe\",
        \"petType\":  \"Pig\",
        \"weight\": 0
    }">>,
    ?assertEqual(expect(?READ_ONLY_ERROR, [<<"weight">>]), test_validate(Pet, ?PET, ?PET_SCHEMA, request)).


-spec ok_read_only_response_test() -> _.
ok_read_only_response_test() ->
    Pet = <<"{
        \"name\":     \"Babe\",
        \"petType\":  \"Pig\",
        \"weight\": 0
    }">>,
    ?assertEqual(ok, test_validate(Pet, ?PET, ?PET_SCHEMA)).

-spec ok_mild_validation_test() -> _.
ok_mild_validation_test() ->
    Pet = <<"{
        \"name\":     \"Rex\",
        \"petType\":  \"Dog\",
        \"packSize\": 2147483650
    }">>,
    ?assertEqual(ok, test_validate(Pet, ?PET, ?PET_SCHEMA, request, #{request => mild})).

-spec ok_no_validation_test() -> _.
ok_no_validation_test() ->
    Pet = <<"{
        \"name\":          true,
        \"petType\":       \"Cool\",
        \"meaningOfLife\": \"42\"
    }">>,
    ?assertEqual(ok, test_validate(Pet, ?PET, ?PET_SCHEMA, request, #{request => none})).
-endif.
