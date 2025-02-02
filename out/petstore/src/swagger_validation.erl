-module(swagger_validation).

-export([prepare_request_param/5]).
-export([validate_response/4]).

-type rule()          :: schema | {required, boolean()} | swagger_param_validator:param_rule().
-type data_type()     :: 'list' | atom().
-type response_spec() :: {data_type(), swagger:param_name()} | undefined.

-type error() :: #{
    type        := error_type(),
    param_name  => swagger:param_name(),
    description => swagger:error_reason()
}.

-type error_type() ::
    no_match        |
    not_found       |
    not_in_range    |
    wrong_length    |
    wrong_size      |
    schema_violated |
    wrong_type |
    wrong_body |
    wrong_array.

-type validation_opts() :: #{
    schema => swagger_schema_validator:schema_validation_opts(),
    custom_validator => module()
}.

-type msg_type() :: request | response.

-export_type([rule/0]).
-export_type([response_spec/0]).
-export_type([error/0]).
-export_type([error_type/0]).
-export_type([validation_opts/0]).
-export_type([msg_type/0]).

-define(catch_error(Block),
    try
        {ok, Block}
    catch
        throw:{wrong_param, Name, Error} ->
            {error, Error#{param_name => Name}}
    end
).


%% API

-spec prepare_request_param(
    OperationID :: swagger:operation_id(),
    Rules :: [rule()],
    Name  :: swagger:param_name(),
    Value :: swagger:value(),
    ValidationOpts :: validation_opts()
) ->
    {ok,    Value :: swagger:value()} |
    {error, Error :: error()}.

prepare_request_param(OperationID, Rules, Name, Value, ValidationOpts) ->
    ?catch_error(validate_param(OperationID, Rules, Name, Value, ValidationOpts)).

-spec validate_response(
    OperationID :: swagger:operation_id(),
    Spec :: response_spec(),
    Resp :: swagger:object() | [swagger:object()] | undefined,
    ValidationOpts :: validation_opts()
) ->
    ok |
    {error, Error :: error()}.

validate_response(OperationID, {DataType, SchemaName}, Body, ValidationOpts) ->
    Result = case DataType of
        'list' ->
            ?catch_error([validate(OperationID, schema, SchemaName, Item, response, ValidationOpts) || Item <- Body]);
        _ ->
            ?catch_error(validate(OperationID, schema, SchemaName, Body, response, ValidationOpts))
    end,
    case Result of
        E = {error, _} -> E;
        _ -> ok
    end;
validate_response(_, undefined, undefined, _) ->
    ok;
validate_response(_, undefined, _, _) ->
    {error, map_error(schema, <<"Must be empty">>)}.


%% Internal

-spec validate_param(
    OperationID :: swagger:operation_id(),
    Rules :: [rule()],
    Name  :: swagger:param_name(),
    Value :: swagger:value(),
    ValidationOpts :: validation_opts()
) ->
    Prepared :: swagger:value() | no_return().

validate_param(OperationID, Rules, Name, Value, ValidationOpts) ->
    lists:foldl(
        fun(Rule, Acc) ->
            case validate(OperationID, Rule, Name, Acc, request, ValidationOpts) of
                ok             -> Acc;
                {ok, Prepared} -> Prepared
            end
        end,
        Value,
        Rules
    ).

validate(_OperationID, Rule = {required, true}, Name, undefined, _MsgType, _ValidationOpts) ->
    report_validation_error(Rule, Name);
validate(_OperationID, {required, _}, _Name, _, _MsgType, _ValidationOpts) ->
    ok;
validate(_OperationID, _, _Name, undefined, _MsgType, _ValidationOpts) ->
    ok;
validate(OperationID, Rule = schema, Name, Value, MsgType, ValidationOpts) ->
    case swagger_schema_validator:validate(OperationID, Value, Name, MsgType, ValidationOpts) of
        ok ->
            ok;
        {error, Reason} ->
            report_validation_error(Rule, Name, Reason)
    end;
validate(OperationID, Rule, Name, Value, MsgType, ValidationOpts) ->
    case swagger_param_validator:validate(OperationID, Rule, Name, Value, MsgType, ValidationOpts) of
        ok ->
            ok;
        Ok = {ok, _} ->
            Ok;
        error ->
            report_validation_error(Rule, Name);
        {error, Reason} ->
            report_validation_error(Rule, Name, Reason)
    end.

-spec report_validation_error(Rule :: rule(), Param :: swagger:param_name()) ->
    no_return().

report_validation_error(Rule, Param) ->
    report_validation_error(Rule, Param, undefined).

-spec report_validation_error(
    Rule        :: rule(),
    Param       :: swagger:param_name(),
    Description :: swagger:error_reason() | undefined
) ->
    no_return().

report_validation_error(Rule, Param, Description) ->
    throw({wrong_param, Param, map_error(Rule, Description)}).

-spec map_error(Rule :: rule(), Description :: swagger:error_reason() | undefined) ->
   Error :: error().
map_error(Rule, Description) ->
    Error = #{type => map_violated_rule(Rule)},
    case Description of
        undefined -> Error;
        _ -> Error#{description => Description}
    end.

-spec map_violated_rule(Rule :: rule()) ->
    ErrorType :: error_type().

map_violated_rule({type, _Type})   ->  wrong_type;
map_violated_rule({format, _Type}) ->  wrong_format;
map_violated_rule({enum, _})       ->  not_in_range;
map_violated_rule({max, _, _})     ->  wrong_size;
map_violated_rule({min, _, _})     ->  wrong_size;
map_violated_rule({max_length, _}) ->  wrong_length;
map_violated_rule({min_length, _}) ->  wrong_length;
map_violated_rule({pattern, _}) ->  no_match;
map_violated_rule(schema) ->  schema_violated;
map_violated_rule({required, _}) ->  not_found;
map_violated_rule({list, _, _}) ->  wrong_array.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec validate_required_test() -> _.

validate_required_test() ->
    ?assertEqual(ok, validate(test, {required, true}, 'Name', <<"test">>, request, strict)),
    ?assertEqual(ok, validate(test, {required, false}, 'Name', <<"test">>, request, strict)),
    ?assertThrow({wrong_param, _, _}, validate(test, {required, true}, 'Name', undefined, request, strict)).

-endif.
