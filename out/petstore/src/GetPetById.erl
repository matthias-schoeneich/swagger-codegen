-module('GetPetById').

-export([handle_request/4]).
handle_request(#{petId := Pet_ID} = _Request, _Context, _Opts,_Whatever) ->

  {ok,  {200,#{},#{<<"id">> => Pet_ID, 
                   <<"category">> => #{<<"id">>   => 2,
                                       <<"name">> => <<"irgendwas">>},
                   <<"name">>     => <<"dagma">>,
                   <<"photoUrls">> => [<<"http bla">>],
                   <<"tags">> => [#{<<"id">> => 3,
                                    <<"name">> => <<"a tag">>}
                                 ],
                   <<"status">> => <<"available">>
                  }
        }}.



