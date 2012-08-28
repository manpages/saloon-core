-module(saloon_http).

-export(
	[return/2
	]
).

-include_lib("cowboy/include/http.hrl").

%%
%% Simple method suitable for simplistic return of HTML.
%%
-spec return({ok, #http_req{}} | binary(), #http_req{}) -> {ok, #http_req{}, return}.
return(Reply, Req) ->
    case Reply of 
        {ok, RepReq} ->
            {ok, RepReq, return};
        HTML ->
            {ok, RenderedRepReq} = 
                cowboy_http_req:reply(
                    200, [], HTML, Req 
                ),  
        {ok, RenderedRepReq, return}
    end.
