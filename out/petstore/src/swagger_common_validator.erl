%% -*- mode: erlang -*-
-module(swagger_common_validator).

-export([validate/2]).

-type param_rule() ::
    {type, 'binary'}  |
    {type, 'integer'} |
    {type, 'boolean'} |
    {type, 'byte'}    |
    {type, 'float'}   |
    %{type, 'list'}    | ?
    %{type, 'map'}     | ?
    %{type, 'object'}  | ?
    %{type, 'file'}    | ?
    {format, 'int32'}      |
    {format, 'int64'}      |
    {format, 'date'}       |
    {format, 'date-time'}  |
    {format, 'email'}      |
    {format, 'ip-address'} |
    {format, 'http-url'}   |
    {format, 'uri'}.

-type result() ::
    ok | {ok, Prepared :: swagger:value()} | error | {error, Message :: term()}.

-export_type([
    param_rule/0,
    result/0
]).

-type value()    :: swagger:value().

%% API

-spec validate(param_rule(), value()) ->
    result().

%% TYPES

validate({type, 'binary'}, Value) ->
    case is_binary(Value) of
        true -> ok;
        false -> error
    end;

validate({type, 'integer'}, Value0) ->
    try
        Value = swagger_utils:to_int(Value0),
        {ok, Value}
    catch
        error:badarg ->
            error
    end;

validate({type, 'boolean'},  Value) when is_boolean(Value) ->
    {ok, Value};
validate({type, 'boolean'}, Value) ->
    case swagger_utils:to_lower(Value) of
        <<"true">> ->  {ok, true};
        <<"false">> -> {ok, false};
        _ -> error
    end;

validate({type, 'byte'}, Value) ->
    try
        validate_base64(Value)
    catch error:badarg ->
        error
    end;

validate({type, 'float'}, Value) ->
    try
        {ok, swagger_utils:to_float(Value)}
    catch
        error:badarg ->
            error
    end;

%% FORMATS

validate({format, 'int64'}, Value0) ->
    try
        Value = swagger_utils:to_int(Value0),
        ok = validate_between(Value, -9223372036854775808, 922337203685477580),
        {ok, Value}
    catch
        error:badarg ->
            error
    end;

validate({format, 'int32'}, Value0) ->
    try
        Value = swagger_utils:to_int(Value0),
        ok = validate_between(Value, -2147483648, 2147483647),
        {ok, Value}
    catch
        error:badarg ->
            error
    end;

validate({format, 'date'}, Value) ->
    case is_binary(Value) of
        true ->
            validate_date(Value);
        false -> error
    end;

validate({format, 'date-time'}, Value) ->
    case is_binary(Value) of
        true ->
            validate_datetime(Value);
        false -> error
    end;

validate({format, 'email'}, Value) when is_binary(Value) ->
    case email_validator:validate(Value) of
        ok -> ok;
        {error, _} -> error
    end;
validate({format, 'email'}, _Value) ->
    error;

validate({format, 'ip-address'}, Value0) when is_binary(Value0) ->
    Value = binary_to_list(Value0),
    case inet:parse_strict_address(Value) of
        {ok, _IPAddress} ->
            ok;
        {error, einval} ->
            error
    end;
validate({format, 'ip-address'}, _Value) ->
    error;

validate({format, 'http-url'}, Value) when is_binary(Value) ->
    case uri_string:parse(Value) of
        #{scheme := Scheme, host := _, path := _} when
            Scheme =:= <<"http">> orelse
            Scheme =:= <<"https">>
        ->
            ok;
        _ ->
            error
    end;

validate({format, 'uri'}, Value) when is_binary(Value) ->
    case uri_string:parse(Value) of
        #{scheme := _, path := _} -> %% Scheme and path a required by RFC3986
            ok;
        _ ->
            error
    end.

%%

-spec validate_between(Value :: swagger:value(), Min :: integer(), Max :: integer()) ->
    ok | no_return().

validate_between(Value, Min, Max) when
    is_integer(Value),
    Value >= Min,
    Value =< Max ->
    ok;

validate_between(_, _, _) ->
    error(badarg).

%% Internal

-spec validate_base64(Value :: swagger:value()) ->
    ok | no_return().

validate_base64(Value) when is_binary(Value) ->
    try
        _ = base64:decode(Value),
        ok
    catch
        _:_ ->
            error(badarg)
    end;

validate_base64(_) ->
    error(badarg).


-spec validate_date(Value :: binary()) ->
    ok | error.

validate_date(Value) when is_binary(Value) ->
    validate_datetime(<<Value/binary, "T00:00:00Z">>).

-spec validate_datetime(Value :: binary()) ->
    ok | error.

validate_datetime(Value) when is_binary(Value) ->
    Str = erlang:binary_to_list(Value),
    try
        _Seconds = calendar:rfc3339_to_system_time(Str),
        ok
    catch
        error:_ ->
            error
    end.
