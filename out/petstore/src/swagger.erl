-module(swagger).
%% Type definitions

%% API

-export_type([request_context/0]).
-export_type([auth_context/0]).
-export_type([client_peer/0]).
-export_type([operation_id/0]).
-export_type([api_key/0]).
-export_type([object/0]).
-export_type([logic_handler/1]).
-export_type([handler_opts/1]).
-export_type([status/0]).
-export_type([headers/0]).
-export_type([response_body/0]).
-export_type([response/0]).

-type auth_context()    :: any().
-type operation_id()    :: atom().
-type api_key()         :: binary().
-type handler_opts(T)   :: T | undefined.
-type logic_handler(T)  :: module() | {module(), handler_opts(T)}.
-type object()          :: map().
-type status()          :: cowboy:http_status().
-type headers()         :: cowboy:http_headers().
-type req()             :: cowboy_req:req().
-type response_body()   :: object() | [object()] | undefined.
-type response()        :: {status(), headers(), response_body()}.

-type client_peer() :: #{
    ip_address  => IP :: inet:ip_address(),
    port_number => Port :: inet:port_number()
}.

-type request_context() :: #{
    auth_context => AuthContext :: auth_context(),
    peer         => client_peer(),
    cowboy_req  => req()
}.


%% Internal

-export_type([param_name/0]).
-export_type([value/0]).
-export_type([error_reason/0]).

-type param_name()    :: atom().
-type value()         :: term().
-type error_reason()  :: binary().
