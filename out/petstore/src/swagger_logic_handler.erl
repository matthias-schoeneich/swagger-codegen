-module(swagger_logic_handler).

-export([handle_request/4]).
-export([map_error/2]).

-type operation_id()    :: swagger:operation_id().
-type api_key()         :: swagger:api_key().
-type auth_context()    :: swagger:auth_context().
-type object()          :: swagger:object().
-type request_context() :: swagger:request_context().
-type handler_opts(T)   :: swagger:handler_opts(T).
-type logic_handler(T)  :: swagger:logic_handler(T).
-type response()        :: swagger:response().

-type validation_error() :: swagger_validation:error().
-type error_type()      :: validation_error.
-type error_message()   :: swagger:error_reason().

-export_type([error_type/0]).

%% Behaviour definition

-export([authorize_api_key/4]).

-callback authorize_api_key(operation_id(), api_key(), request_context(), handler_opts(_)) ->
    boolean() | {boolean(), auth_context()}.

-callback handle_request(operation_id(), object(), request_context(), handler_opts(_)) ->
    {ok | error, response()}.

-callback map_error(error_type(), validation_error()) ->
    error_message().

%% API

-spec handle_request(logic_handler(_), operation_id(), object(), request_context()) ->
    {ok | error, response()}.

handle_request(Handler, OperationID, Request, Context) ->
    {Module, Opts} = get_mod_opts(Handler),
    Module:handle_request(OperationID, Request, Context, Opts).

-spec map_error(module(), {error_type(), validation_error()}) ->
    error_message().

map_error(Handler, {Type, Error}) ->
    {Module, _Opts} = get_mod_opts(Handler),
    Module:map_error(Type, Error).

-spec authorize_api_key(logic_handler(_), operation_id(), api_key(), request_context()) ->
    false | {true, auth_context()}.
authorize_api_key(Handler, OperationID, ApiKey, Context) ->
    {Module, Opts} = get_mod_opts(Handler),
    Module:authorize_api_key(OperationID, ApiKey, Context, Opts).

%% Internal functions

get_mod_opts(ModOpts= {_, _}) ->
    ModOpts;
get_mod_opts(Module) ->
    {Module, undefined}.
