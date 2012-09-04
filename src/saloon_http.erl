-module(saloon_http).

-export([
		 return/2
		,receive_file/2
	]).


-include_lib("cowboy/include/http.hrl").
-include_lib("saloon/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%%
%% Handling file upload. Thanks, yrashk, for sharing that snippet!
%%
receive_file(Req@, State) ->
	case cowboy_http_req:multipart_data(Req@) of
		{error, badarg} ->
			?debugMsg("No multipart data, 500"),
			{ok, Req@} = cowboy_http_req:reply(500, Req@),
			{Req@, State};
		{{headers, Headers}, _Req@} ->
			?debugMsg("Parsing headers"),
			Disp = cowboy_multipart:content_disposition(proplists:get_value(<<"Content-Disposition">>, Headers)),
			case kvc:path('form-data'.filename, [Disp]) of
				[] -> %% not a file
					?debugMsg("Not a file"),
					?debugFmt("Dumping form-data: ~n~p~n", [kvc:path('form-data', [Disp])]),
					{ok, Req@} = cowboy_http_req:reply(500, Req@),
					{Req@, State};
				FilenameB ->
					?debugFmt("File ~p", [FilenameB]),
					ContentType = proplists:get_value('Content-Type', Headers,
													  <<"application/octet-stream">>),
					Filename = binary_to_list(FilenameB),
					{ok, Uploader} = saloon_uploader:start(FilenameB, ContentType),
					receive_file(Req@, 
							 State#upload_state{ 
							   filename = Filename,
							   content_type = ContentType,
							   uploader = Uploader
							  })
			end;
		{{body, Data}, Req@} ->
			?debugMsg("Got body"),
			if 
				State#upload_state.uploader /= undefined ->
					?debugFmt("Uploading through uploader at ~p", [State#upload_state.uploader]),
					saloon_uploader:write(State#upload_state.uploader, Data),
					receive_file(Req@, State#upload_state{ size = size(Data) + State#upload_state.size });
				true ->
					?debugMsg("Wtf?"),
					receive_file(Req@, State)
			end;
		{end_of_part, Req@} ->
			?debugMsg("Got end of part"),
			if
				State#upload_state.uploader /= undefined ->
					case saloon_uploader:done(State#upload_state.uploader) of
						{ok, URL, Location} ->
							?debugFmt("Finalizing upload at ~p", [State#upload_state.uploader]),
							Files = [[
									  {name, State#upload_state.filename},
									  {url, URL}]|State#upload_state.files],
							receive_file(Req@, State#upload_state{ filename = undefined,
													content_type = undefined,
													uploader = undefined,
													size = 0,
													files = Files
												  });
						_Error ->
							{ok, Req@} = cowboy_http_req:reply(500, Req@),
							{Req@, State}
					end;
				true ->
					?debugMsg("Wtf? end_of_part"),
					receive_file(Req@, State)
			end;
		{eof, Req@} ->
			?debugMsg("Got EOF"),
			{ok, Req@} = cowboy_http_req:reply(200, 
											   [{<<"Content-Type">>, <<"application/json">>}],
											   jsx:to_json(State#upload_state.files),
											   Req@),
			{Req@, State#upload_state{ files = [] }}
	end.

