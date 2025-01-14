%% -*- mode: erlang -*-

%% basic handler
-module(swagger_user_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: swagger:operation_id(),
    logic_handler :: module(),
    swagger_handler_opts :: swagger_router:swagger_handler_opts(),
    context       :: swagger:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {stop, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(Req :: cowboy_req:req(), Opts :: swagger_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {_Operations, LogicHandler, SwaggerHandlerOpts} = InitOpts) ->
    OperationID    = swagger_utils:get_operation_id(Req, InitOpts),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = #{cowboy_req => Req}
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateUser'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateUsersWithArrayInput'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateUsersWithListInput'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'DeleteUser'
    }
) ->
    {[<<"DELETE">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetUserByName'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'LoginUser'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'LogoutUser'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'UpdateUser'
    }
) ->
    {[<<"PUT">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(
    Req,
    State = #state{
        operation_id  = 'CreateUser'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'CreateUsersWithArrayInput'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'CreateUsersWithListInput'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'DeleteUser'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'GetUserByName'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'LoginUser'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'LogoutUser'
    }
) ->
    {true, Req, State};

is_authorized(
    Req,
    State = #state{
        operation_id  = 'UpdateUser'
    }
) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateUser'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateUsersWithArrayInput'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateUsersWithListInput'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'DeleteUser'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetUserByName'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'LoginUser'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'LogoutUser'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'UpdateUser'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request_json}
    ], Req, State}.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State = #state{context = Context}) ->
    PeerResult = swagger_handler_api:determine_peer(Req),
    case PeerResult of
        {ok, Peer} ->
            Context1 = Context#{peer => Peer},
            State1   = State#state{context = Context1},
            {false, Req, State1};
        {error, Reason} ->
            error_logger:error_msg("Unable to determine client peer: ~p", [Reason]),
            {true, Req, State}
    end.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = Context
    }
) ->
    ValidationOpts = maps:get(validation_opts, SwaggerHandlerOpts, #{}),
    case populate_request(LogicHandler, OperationID, Req0, ValidationOpts) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok = validate_response(Status, Resp, OperationID, ValidationOpts),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(LogicHandler, OperationID, Req, ValidationOpts) ->
    Spec = get_request_spec(OperationID),
    swagger_handler_api:populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    swagger_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID, ValidationOpts) ->
    Spec = get_response_spec(OperationID, Code),
    swagger_handler_api:validate_response(OperationID, Spec, Body, ValidationOpts).

encode_response(Resp) ->
    swagger_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = swagger_handler_api:process_response(Status, Result, Req0, OperationID),
    {stop, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: swagger:operation_id()) ->
    Spec :: swagger_handler_api:request_spec() | no_return().


get_request_spec('CreateUser') ->
    [
        {'User', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ];
get_request_spec('CreateUsersWithArrayInput') ->
    [
        {'list', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ];
get_request_spec('CreateUsersWithListInput') ->
    [
        {'list', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ];
get_request_spec('DeleteUser') ->
    [
        {'username', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }}
    ];
get_request_spec('GetUserByName') ->
    [
        {'username', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }}
    ];
get_request_spec('LoginUser') ->
    [
        {'username', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }},
        {'password', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }}
    ];
get_request_spec('LogoutUser') ->
    [
        
    ];
get_request_spec('UpdateUser') ->
    [
        {'username', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }},
        {'User', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ].

-spec get_response_spec(OperationID :: swagger:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: swagger_handler_api:response_spec() | no_return().


get_response_spec('CreateUser', 0) ->
    undefined;

get_response_spec('CreateUsersWithArrayInput', 0) ->
    undefined;

get_response_spec('CreateUsersWithListInput', 0) ->
    undefined;

get_response_spec('DeleteUser', 400) ->
    undefined;

get_response_spec('DeleteUser', 404) ->
    undefined;

get_response_spec('GetUserByName', 200) ->
    {'User', 'User'};

get_response_spec('GetUserByName', 400) ->
    undefined;

get_response_spec('GetUserByName', 404) ->
    undefined;

get_response_spec('LoginUser', 200) ->
    {'binary', 'string'};

get_response_spec('LoginUser', 400) ->
    undefined;

get_response_spec('LogoutUser', 0) ->
    undefined;

get_response_spec('UpdateUser', 400) ->
    undefined;

get_response_spec('UpdateUser', 404) ->
    undefined;

get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).
