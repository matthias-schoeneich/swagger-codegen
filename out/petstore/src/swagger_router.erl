-module(swagger_router).

-export([get_paths/1]).
-export([get_paths/2]).
-export([get_operation/1]).
-export([get_operations/0]).

-type operations() :: #{
    Method :: binary() => OperationID :: swagger:operation_id()
}.

-type logic_handler(T) :: swagger:logic_handler(T).

-type swagger_handler_opts() :: #{
    validation_opts => swagger_validation:validation_opts()
}.

-type init_opts() :: {
    Operations      :: operations(),
    LogicHandler    :: logic_handler(_),
    SwaggerHandlerOpts :: swagger_handler_opts()
}.

-type operation_spec() :: #{
    path    := '_' | iodata(),
    method  := binary(),
    handler := module()
}.

-export_type([swagger_handler_opts/0]).
-export_type([init_opts/0]).
-export_type([operation_spec/0]).

-spec get_paths(LogicHandler :: logic_handler(_)) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    get_paths(LogicHandler, #{}).

-spec get_paths(LogicHandler :: logic_handler(_), SwaggerHandlerOpts :: swagger_handler_opts()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler, SwaggerHandlerOpts) ->
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, SwaggerHandlerOpts}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

-spec get_operation(swagger:operation_id()) ->
   operation_spec().

get_operation(OperationId) ->
    maps:get(OperationId, get_operations()).

-spec get_operations() -> #{
    swagger:operation_id() := operation_spec()
}.

get_operations() ->
    #{ 
        'AddPet' => #{
            path => "/v2/pet",
            method => <<"POST">>,
            handler => 'swagger_pet_handler'
        },
        'DeletePet' => #{
            path => "/v2/pet/:petId",
            method => <<"DELETE">>,
            handler => 'swagger_pet_handler'
        },
        'FindPetsByStatus' => #{
            path => "/v2/pet/findByStatus",
            method => <<"GET">>,
            handler => 'swagger_pet_handler'
        },
        'FindPetsByTags' => #{
            path => "/v2/pet/findByTags",
            method => <<"GET">>,
            handler => 'swagger_pet_handler'
        },
        'GetPetById' => #{
            path => "/v2/pet/:petId",
            method => <<"GET">>,
            handler => 'swagger_pet_handler'
        },
        'UpdatePet' => #{
            path => "/v2/pet",
            method => <<"PUT">>,
            handler => 'swagger_pet_handler'
        },
        'UpdatePetWithForm' => #{
            path => "/v2/pet/:petId",
            method => <<"POST">>,
            handler => 'swagger_pet_handler'
        },
        'UploadFile' => #{
            path => "/v2/pet/:petId/uploadImage",
            method => <<"POST">>,
            handler => 'swagger_pet_handler'
        },
        'DeleteOrder' => #{
            path => "/v2/store/order/:orderId",
            method => <<"DELETE">>,
            handler => 'swagger_store_handler'
        },
        'GetInventory' => #{
            path => "/v2/store/inventory",
            method => <<"GET">>,
            handler => 'swagger_store_handler'
        },
        'GetOrderById' => #{
            path => "/v2/store/order/:orderId",
            method => <<"GET">>,
            handler => 'swagger_store_handler'
        },
        'PlaceOrder' => #{
            path => "/v2/store/order",
            method => <<"POST">>,
            handler => 'swagger_store_handler'
        },
        'CreateUser' => #{
            path => "/v2/user",
            method => <<"POST">>,
            handler => 'swagger_user_handler'
        },
        'CreateUsersWithArrayInput' => #{
            path => "/v2/user/createWithArray",
            method => <<"POST">>,
            handler => 'swagger_user_handler'
        },
        'CreateUsersWithListInput' => #{
            path => "/v2/user/createWithList",
            method => <<"POST">>,
            handler => 'swagger_user_handler'
        },
        'DeleteUser' => #{
            path => "/v2/user/:username",
            method => <<"DELETE">>,
            handler => 'swagger_user_handler'
        },
        'GetUserByName' => #{
            path => "/v2/user/:username",
            method => <<"GET">>,
            handler => 'swagger_user_handler'
        },
        'LoginUser' => #{
            path => "/v2/user/login",
            method => <<"GET">>,
            handler => 'swagger_user_handler'
        },
        'LogoutUser' => #{
            path => "/v2/user/logout",
            method => <<"GET">>,
            handler => 'swagger_user_handler'
        },
        'UpdateUser' => #{
            path => "/v2/user/:username",
            method => <<"PUT">>,
            handler => 'swagger_user_handler'
        }
    }.
