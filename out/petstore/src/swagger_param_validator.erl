%% -*- mode: erlang -*-
-module(swagger_param_validator).

-export([validate/6]).

-type param_base_rule() ::
    swagger_common_validator:param_rule() |
    {enum, [atom()]} |
    {max, Max :: number(), Type :: exclusive | inclusive} |
    {min, Min :: number(), Type :: exclusive | inclusive} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: iodata() | unicode:charlist()} |
    boolean().
-type collection_format() ::
    'csv' |
    'ssv' |
    'tsv' |
    'pipes'.
-type param_rule() ::
    param_base_rule() |
    {'list', collection_format(), [param_base_rule()]}.

-type operation_id() :: swagger:operation_id().
-type name()     :: swagger:param_name().
-type value()    :: swagger:value().
-type msg_type() :: swagger_validation:msg_type().

-type result() :: ok | {ok, Prepared :: value()} | error | {error, Error :: term()}.

-type validation_opts() :: swagger_validation:validation_opts().

-type context() :: #{
    name := name(),
    msg_type := msg_type(),
    operation_id := operation_id()
}.

-export_type([param_rule/0]).
-export_type([context/0]).

%% API

-spec validate(operation_id(), param_rule(), name(), value(), msg_type(), validation_opts()) ->
    result().

validate(OperationID, Rule, Name, Value, MsgType, ValidationOpts) ->
    Meta = make_context(OperationID, Name, MsgType),
    validate_with_context(Rule, Value, Meta, ValidationOpts).

validate_with_context(Rule, Value, Meta, ValidationOpts) ->
    case swagger_custom_validator:validate_param(Rule, Value, Meta, ValidationOpts) of
        pass -> do_validate(Rule, Value, Meta, ValidationOpts);
        Result -> Result
    end.

%%

-spec do_validate(param_rule(), value(), context(), validation_opts()) ->
    result().

do_validate(true, _Value, _Meta, _Opts) ->
    ok;

do_validate(false, _Value, _Meta, _Opts) ->
    error;

do_validate({'list', Format, Ruleset}, Value, Meta, Opts) ->
    try
        Values = parse_array(Format, Value),
        {ok, [validate_ruleset(Ruleset, V, Meta, Opts) || V <- Values]}
    catch
        _:_ ->
            error
    end;

do_validate({enum, Values}, Value, _Meta, _Opts) ->
    try
        FormattedValue = swagger_utils:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> error
        end
    catch
        error:badarg ->
            error
    end;

do_validate({max, Max, Type}, Value, _Meta, _Opts) ->
    Result = case Value of
        _ when Value < Max andalso Type =:= exclusive ->
            true;
        _ when Value =< Max andalso Type =:= inclusive  ->
            true;
        _ ->
            false
    end,
    case Result of
        true -> ok;
        false -> error
    end;

do_validate({min, Min, Type}, Value, _Meta, _Opts) ->
    Result = case Value of
        _ when Value > Min andalso Type =:= exclusive ->
            true;
        _ when Value >= Min andalso Type =:= inclusive ->
            true;
        _ ->
            false
    end,
    case Result of
        true -> ok;
        false -> error
    end;

do_validate({max_length, MaxLength}, Value, _Meta, _Opts) ->
    case size(Value) =< MaxLength of
        true -> ok;
        false -> error
    end;

do_validate({min_length, MinLength}, Value, _Meta, _Opts) ->
    case size(Value) >= MinLength of
        true -> ok;
        false -> error
    end;

do_validate({pattern, Pattern}, Value, _Meta, _Opts) ->
    {ok, MP} = re:compile(Pattern, [unicode, ucp]),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> error
    end;

% Common

do_validate({RuleName, _} = Rule, Value, _Meta, _Opts) when
    RuleName =:= type;
    RuleName =:= format
->
    swagger_common_validator:validate(Rule, Value).

%% Internal

make_context(OperationID, Name, MsgType) ->
    #{
        operation_id => OperationID,
        name => Name,
        msg_type => MsgType
    }.

-spec validate_ruleset(
    Ruleset :: [param_base_rule()],
    Value   :: swagger:value(),
    Meta    :: context(),
    Opts    :: validation_opts()
) ->
    Value   :: swagger:value().

validate_ruleset(Ruleset, Value, Meta, Opts) ->
    lists:foldl(
        fun(R, V0) ->
            case validate_with_context(R, Value, Meta, Opts) of
                {ok, V} -> V;
                ok -> V0;
                error -> throw(wrong_param)
            end
        end,
        Value,
        Ruleset
    ).

-spec parse_array(
    Format :: collection_format(),
    Array  :: binary()
) ->
    Values :: [binary()].

parse_array(Format, Array) ->
    binary:split(Array, get_split_pattern(Format), [global]).

get_split_pattern('csv') ->
    <<",">>;
get_split_pattern('ssv') ->
    <<" ">>;
get_split_pattern('tsv') ->
    <<"\t">>;
get_split_pattern('pipes') ->
    <<"|">>.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec validate_integer_test() -> _.
-spec validate_int64_test() -> _.
-spec validate_int32_test() -> _.
-spec validate_float_test() -> _.
-spec validate_binary_test() -> _.
-spec validate_byte_test() -> _.
-spec validate_boolean_test() -> _.
-spec validate_date_test() -> _.
-spec validate_datetime_test() -> _.
-spec validate_email_test() -> _.
-spec validate_ip_address_test() -> _.
-spec validate_uri_test() -> _.
-spec validate_http_url_test() -> _.
-spec validate_enum_test() -> _.
-spec validate_max_test() -> _.
-spec validate_min_test() -> _.
-spec validate_max_length_test() -> _.
-spec validate_min_length_test() -> _.
-spec validate_pattern_test() -> _.
-spec validate_array_test() -> _.

validate_integer_test() ->
    ?assertEqual({ok, 2}, test_validate({type, 'integer'}, 2)),
    ?assertEqual({ok, 6}, test_validate({type, 'integer'}, <<"6">>)),
    ?assertEqual(error,   test_validate({type, 'integer'}, <<"nope">>)).

validate_int64_test() ->
    ?assertEqual({ok, 2}, test_validate({format, 'int64'},2)),
    ?assertEqual({ok, 6}, test_validate({format, 'int64'},<<"6">>)),
    ?assertEqual(error, test_validate({format, 'int64'}, 922337203685477581)),
    ?assertEqual(error, test_validate({format, 'int64'},-9223372036854775809)).

validate_int32_test() ->
    ?assertEqual({ok, 6}, test_validate({format, 'int32'}, 6)),
    ?assertEqual({ok, 21}, test_validate({format, 'int32'}, <<"21">>)),
    ?assertEqual(error, test_validate({format, 'int32'}, -2147483649)),
    ?assertEqual(error, test_validate({format, 'int32'},2147483648)).

validate_float_test() ->
    ?assertEqual({ok, 1.9}, test_validate({type, 'float'}, <<"1.9">>)),
    ?assertEqual({ok, 3.0}, test_validate({type, 'float'}, <<"3">>)),
    ?assertEqual(error, test_validate({type, 'float'}, <<"c">>)).

validate_binary_test() ->
    ?assertEqual(ok, test_validate({type, 'binary'}, <<"f">>)),
    ?assertEqual(error, test_validate({type, 'binary'}, [])),
    ?assertEqual(error, test_validate({type, 'binary'}, 3)).

validate_byte_test() ->
    ?assertEqual(ok, test_validate({type, 'byte'}, <<"0YXRg9C5">>)),
    ?assertEqual(error, test_validate({type, 'byte'}, <<"g">>)).

validate_boolean_test() ->
    ?assertEqual({ok, true}, test_validate({type, 'boolean'}, <<"true">>)),
    ?assertEqual({ok, false}, test_validate({type, 'boolean'}, <<"false">>)),
    ?assertEqual({ok, false}, test_validate({type, 'boolean'}, false)),
    ?assertEqual(error, test_validate({type, 'boolean'}, <<"nope">>)).

validate_date_test() ->
    ?assertEqual(ok, test_validate({format, 'date'}, <<"2014-03-19">>)),
    ?assertEqual(error, test_validate({format, 'date'}, <<"2014-19-03">>)),
    ?assertEqual(error, test_validate({format, 'date'}, <<"2013">>)),
    ?assertEqual(error, test_validate({format, 'date'}, <<"nope">>)),
    ?assertEqual(error, test_validate({format, 'date'}, <<"2014-03-19 18:00:05-04:00">>)).

validate_datetime_test() ->
    ?assertEqual(ok, test_validate({format, 'date-time'}, <<"2014-03-19T18:35:03-04:00">>)),
    ?assertEqual(error, test_validate({format, 'date-time'}, <<"2014-11-19">>)),
    ?assertEqual(error, test_validate({format, 'date-time'}, <<"nope">>)).

validate_email_test() ->
    ?assertEqual(ok, test_validate({format, 'email'}, <<"me@example.com">>)),
    ?assertEqual(error, test_validate({format, 'email'}, <<"m\\e@example.com">>)),
    ?assertEqual(error, test_validate({format, 'email'}, <<"nope">>)).

validate_ip_address_test() ->
    ?assertEqual(ok, test_validate({format, 'ip-address'}, <<"127.0.0.1">>)),
    ?assertEqual(ok, test_validate({format, 'ip-address'}, <<"::1">>)),
    ?assertEqual(error, test_validate({format, 'ip-address'}, <<"127.0.0.0.1">>)),
    ?assertEqual(error, test_validate({format, 'ip-address'}, <<"nope">>)).

validate_uri_test() ->
    ?assertEqual(ok, test_validate({format, 'uri'}, <<"http://localhost">>)),
    ?assertEqual(ok, test_validate({format, 'uri'}, <<"ftp://user@host:21/home">>)),
    ?assertEqual(error, test_validate({format, 'uri'}, <<"127.0.0.1">>)),
    ?assertEqual(error, test_validate({format, 'uri'}, <<"example.com">>)).

validate_http_url_test() ->
    ?assertEqual(ok, test_validate({format, 'http-url'}, <<"http://localhost">>)),
    ?assertEqual(error, test_validate({format, 'http-url'}, <<"ftp://user@host:21/home">>)).

validate_enum_test() ->
    ?assertEqual({ok, sad}, test_validate({enum, [i, am, sad]} , <<"sad">>)),
    ?assertEqual(error, test_validate({enum, ['All work and no play', 'makes Jack a dull boy']}, <<"Artem">>)),
    ?assertEqual(error, test_validate({enum, []}, <<"">>)).

validate_max_test() ->
    ?assertEqual(ok, test_validate({max, 10, inclusive}, 10)),
    ?assertEqual(error, test_validate({max, 10, exclusive}, 10)),
    ?assertEqual(ok, test_validate({max, 32, inclusive}, 21)),
    ?assertEqual(ok, test_validate({max, 32, exclusive}, 21)).

validate_min_test() ->
    ?assertEqual(ok, test_validate({min, 33, inclusive}, 33)),
    ?assertEqual(error, test_validate({min, 33, exclusive}, 33)),
    ?assertEqual(ok, test_validate({min, 57, inclusive}, 60)),
    ?assertEqual(ok, test_validate({min, 57, inclusive}, 60)).

validate_max_length_test() ->
    ?assertEqual(ok, test_validate({max_length, 5}, <<"hello">>)),
    ?assertEqual(ok, test_validate({max_length, 5}, <<"h">>)),
    ?assertEqual(error, test_validate({max_length, 5}, <<"hello?">>)).

validate_min_length_test() ->
    ?assertEqual(ok, test_validate({min_length, 5}, <<"hello">>)),
    ?assertEqual(ok, test_validate({min_length, 5}, <<"hello?">>)),
    ?assertEqual(error, test_validate({min_length, 5}, <<"h">>)).

validate_pattern_test() ->
    ?assertEqual(ok, test_validate({pattern, <<"[abc]">>},  <<"adcv">>)),
    ?assertEqual(error, test_validate({pattern, <<"[abc]">>},  <<"fgh0">>)),
    ?assertEqual(ok, test_validate({pattern, <<"^[0-9]{2}\/[0-9]{2}$">>}, <<"22/22">>)),
    ?assertEqual(error, test_validate({pattern, <<"^[0-9]{2}\/[0-9]{2}$">>}, <<"22/225">>)).

validate_array_test() ->
    ?assertEqual({ok, [10,11,12]}, test_validate({list, 'csv', [{format, 'int32'}]}, <<"10,11,12">>)),
    ?assertEqual(error, test_validate({list, 'csv', [{format, 'int32'}]}, <<"10,xyi,12">>)).

test_validate(Rule, Value) ->
    do_validate(Rule, Value, #{name => 'Test', msg_type => request, operation_id => undefined}, #{}).

-endif.
