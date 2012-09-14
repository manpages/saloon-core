%%
%% Setting uid, ulang and other uglyhacks
%%

-module(saloon_init).

-export([prepare/1]).

%-define(DEV_MODE, true).

-include_lib("eunit/include/eunit.hrl").

prepare(Req) ->
	ensure_fission(),
	UID = case saloon_util:ck(<<"auth">>, Req) of 
		undefined -> 0;
		Cookie -> case saloon_auth:from_cookie(Cookie) of
			UID2 when is_integer(UID2) -> UID2;
			_ -> 0
		end
	end,
	
	Lang = case saloon_util:ck(<<"lang">>, Req) of
		<<"lv">> -> lv;
		<<"ru">> -> ru;
		%% ...
		_        -> en
	end,

	put(user, UID),      %% need to force
	put(language, Lang), %% changes here.
	case init:get_argument(devmode) of
		{ok, _} -> 
			print_request_info(Req),
			print_user_info(UID, Lang),
			case make:all([load]) of
				up_to_date ->
					ok;
				error ->
					erlang:error(recompile_error)
			end; %dirty-dirty
		_ -> ok
	end,
	ok.

%<---------------------cut here----------------------------

print_request_info(Req) ->
	case cowboy_http_req:parse_header('Content-Type', Req) of 
		{undefined, _} ->
			?debugFmt("~n-REQUEST---- ~p ---~nPath: ~p~nPeer: ~p~nBody query string data:~p~n", 
				[X || {X, _} <- [
					 cowboy_http_req:method(Req)
					,cowboy_http_req:raw_path(Req)
					,cowboy_http_req:peer(Req)
					,cowboy_http_req:body_qs(Req)]]
			);
		_ -> 
			?debugFmt("~n-MULTIPART REQUEST----~nPath: ~p~nPeer: ~p~n", 
				[X || {X, _} <- [
					 cowboy_http_req:raw_path(Req)
					,cowboy_http_req:peer(Req)]]
			)
	end
end.

print_user_info(UID, Lang) ->
	?debugFmt("uid -> ~p; lang -> ~p~n", [UID, Lang]).

ensure_fission() ->	
	try 
		case fission_syn:get(storage_node) of
			flase -> fission_syn:set(storage_node, node());
			{value, _} -> ok
		end
	catch
		_Error:_Reason -> 
			fission:initialize(node()),
			fission_syn:set(storage_node, node()),
			case init:get_argument(devmode) of
				{ok, _} ->
					lists:foreach(fun({Key, Value}) -> 
						fission_syn:set(Key, Value)
					end, saloon_conf:initial_testing_data());
				_ ->
					lists:foreach(fun({Key, Value}) -> 
							fission_syn:set(Key, Value)
						end, saloon_conf:initial_data())
			end
	end.
