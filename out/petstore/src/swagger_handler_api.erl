%% -*- mode: erlang -*-
-module(swagger_handler_api).

-export([authorize_api_key/6]).
-export([determine_peer/1]).
-export([populate_request/5]).
-export([validate_response/4]).
-export([encode_response/1]).
-export([process_response/4]).

%%
-type param_source() ::
    qs_val  |
    binding |
    header  |
    body.

-type request_spec() :: [{
    swagger:param_name(),
    #{source := param_source(), rules := [swagger_validation:rule()]}
}].

-type response_spec() :: swagger_validation:response_spec().

-export_type([param_source/0]).
-export_type([request_spec/0]).
-export_type([response_spec/0]).

-type response() :: {cowboy:http_status(), cowboy:http_headers(), binary() | undefined}.

%% API

-spec authorize_api_key(
    LogicHandler :: module(),
    OperationID  :: swagger:operation_id(),
    From         :: header | qs_val,
    KeyParam     :: iodata() | atom(),
    Req          :: cowboy_req:req(),
    Context      :: swagger:request_context()
)->
    {true, swagger:auth_context(), cowboy_req:req()} |
    {false, AuthHeader :: binary(), cowboy_req:req()}.

authorize_api_key(LogicHandler, OperationID, From, KeyParam, Req0, Context) ->
    {ok, ApiKey, Req} = get_value(From, KeyParam, Req0),
    case ApiKey of
        undefined ->
            AuthHeader = <<"">>,
            {false, AuthHeader, Req};
        _ ->
            Result = swagger_logic_handler:authorize_api_key(
                LogicHandler,
                OperationID,
                ApiKey,
                Context
            ),
            case Result of
                {true, AuthContext}  ->
                    {true, AuthContext, Req};
                false ->
                    AuthHeader = <<"">>,
                    {false, AuthHeader, Req}
            end
    end.

-spec determine_peer(Req :: cowboy_req:req()) ->
        {ok, Peer :: swagger:client_peer()} | {error, Reason :: malformed | einval}.

determine_peer(Req) ->
    Peer  = cowboy_req:peer(Req),
    Value = cowboy_req:header(<<"x-forwarded-for">>, Req),
    determine_peer_from_header(Value, Peer).

-spec populate_request(
    LogicHandler :: module(),
    OperationID :: swagger:operation_id(),
    Spec :: request_spec(),
    Req :: cowboy_req:req(),
    ValidationOpts :: swagger_validation:validation_opts()
) ->
    {ok,    Populated :: swagger:object(),       Req :: cowboy_req:req()} |
    {error, Message   :: swagger:error_reason(), Req :: cowboy_req:req()}.

populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts) ->
    populate_request(LogicHandler, OperationID, Spec, Req, #{}, ValidationOpts).

-spec validate_response(
    OperationID :: swagger:operation_id(),
    Spec     :: response_spec(),
    RespBody :: swagger:object() | [swagger:object()] | undefined,
    ValidationOpts :: swagger_validation:validation_opts()
) ->
    ok | no_return().

validate_response(OperationID, Spec, RespBody, ValidationOpts) ->
    case swagger_validation:validate_response(OperationID, Spec, RespBody, ValidationOpts) of
        ok ->
            ok;
        {error, Error} ->
            erlang:error({response_validation_failed, Error, RespBody})
    end.

-spec encode_response(Resp :: swagger:response()) ->
    Encoded :: response().

encode_response(Resp = {_, _, undefined}) ->
    Resp;
encode_response({Code, Headers, Body}) ->
    {Code, Headers, jsx:encode(Body)}.

-spec process_response(
    Status      :: ok | error,
    Result      :: response() | swagger:error_reason(),
    Req         :: cowboy_req:req(),
    OperationID :: swagger:operation_id()
) ->
    Req :: cowboy_req:req().

process_response(ok, {Code, Headers, undefined}, Req, _) ->
    cowboy_req:reply(Code, Headers, Req);
process_response(ok, {Code, Headers, Body}, Req, _) ->
    cowboy_req:reply(Code, Headers, Body, Req);
process_response(error, Message, Req, OperationID) ->
    error_logger:info_msg(
        "Unable to process request for ~p: ~ts",
        [OperationID, Message]
    ),
    cowboy_req:reply(400, #{}, Message, Req).


%% Internal

populate_request(_LogicHandler, _OperationID, [], Req, Populated, _ValidationOpts) ->
    {ok, Populated, Req};
populate_request(LogicHandler, OperationID, [ParamSpec | T], Req0, Populated, ValidationOpts) ->
    case populate_request_param(LogicHandler, OperationID, ParamSpec, Req0, ValidationOpts) of
        {ok, K, V, Req} ->
            populate_request(LogicHandler, OperationID, T, Req, maps:put(K, V, Populated), ValidationOpts);
        Error ->
            Error
    end.

populate_request_param(LogicHandler, OperationID, {Name, #{rules := Rules, source := Source}}, Req0, ValidationOpts) ->
    case get_value(Source, Name, Req0) of
        {ok, Value, Req} ->
            case swagger_validation:prepare_request_param(OperationID, Rules, Name, Value, ValidationOpts) of
                {ok, Result} ->
                    {ok, Name, Result, Req};
                {error, Error}  ->
                    {error, swagger_logic_handler:map_error(LogicHandler, {validation_error, Error}), Req}
            end;
        {error, Message, Req} ->
            Error = #{
                type => wrong_body,
                param_name => Name,
                description => Message
            },
            {error, swagger_logic_handler:map_error(LogicHandler, {validation_error, Error}), Req}
    end.

determine_peer_from_header(undefined, {IP, Port}) ->
    % undefined, assuming no proxies were involved
    {ok, #{ip_address => IP, port_number => Port}};
determine_peer_from_header(Value, _Peer) when is_binary(Value) ->
    ClientPeer = string:strip(binary_to_list(Value)),
    case string:tokens(ClientPeer, ", ") of
        [ClientIP | _Proxies] ->
            case inet:parse_strict_address(ClientIP) of
                {ok, IP} ->
                    % ok
                    {ok, #{ip_address => IP}};
                Error ->
                    % unparseable ip address
                    Error
            end;
        _ ->
            % empty or malformed value
            {error, malformed}
    end.

-spec get_value(
    Source :: param_source(),
    Name   :: swagger:param_name(),
    Req    :: cowboy_req:req()
) ->
    {ok,    Value   :: swagger:value(),        Req :: cowboy_req:req()} |
    {error, Message :: swagger:error_reason(), Req :: cowboy_req:req()}.

get_value(body, _Name, Req0) ->
    case swagger_utils:get_body(Req0) of
        {ok, Body, Req} ->
            case decode_body(Body) of
                {ok, Value} ->
                    {ok, Value, Req};
                {error, Message} ->
                    {error, Message, Req}
            end;
        {error, Message} ->
            {error, Message, Req0}
    end;
get_value(qs_val, Name, Req) ->
    try cowboy_req:match_qs([{Name, [], undefined}], Req) of
        #{Name := Value} ->
            {ok, Value, Req}
    catch
        exit:{request_error, _, _} ->
            {error, <<"Invalid query">>, Req}
    end;
get_value(header, Name, Req) ->
    Headers = cowboy_req:headers(Req),
    {ok, maps:get(swagger_utils:to_header(Name), Headers, undefined), Req};
get_value(binding, Name, Req) ->
    Bindings = cowboy_req:bindings(Req),
    {ok, maps:get(swagger_utils:to_binding(Name), Bindings, undefined), Req}.

-spec decode_body(Body :: binary()) ->
    {ok,    Decoded :: swagger:object() | undefined} |
    {error, Message :: swagger:error_reason()}.
decode_body(<<>>) ->
    {ok, undefined};
decode_body(Body) ->
    try
        {ok, jsx:decode(Body, [return_maps])}
    catch
        _:_ ->
            {error, <<"Invalid json">>}
    end.
