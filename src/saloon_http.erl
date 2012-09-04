-module(saloon_http).

-export([
		 return/2
	]).


-include_lib("cowboy/include/http.hrl").
-include_lib("saloon/include/file.hrl").

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
			{ok, Req@} = cowboy_http_req:reply(500, Req@),
			{Req@, State};
		{{headers, Headers}, Req@} ->
			Disp = cowboy_multipart:content_disposition(proplists:get_value(<<"Content-Disposition">>, Headers)),
			case kvc:path('form-data'.filename, [Disp]) of
				[] -> %% not a file
					receive_file(Req@, State);
				FilenameB ->
					ContentType = proplists:get_value('Content-Type', Headers,
													  <<"application/octet-stream">>),
					Filename = binary_to_list(FilenameB),
					{ok, Uploader} = file_uploader:start(FilenameB,
																ContentType),
					receive_file(Req@, 
							 State#state{ 
							   filename = Filename,
							   content_type = ContentType,
							   uploader = Uploader
							  })
			end;
		{{body, Data}, Req@} ->
			if 
				State#state.uploader /= undefined ->
					file_uploader:write(State#state.uploader, Data),
					receive_file(Req@, State#state{ size = size(Data) + State#state.size });
				true ->
					receive_file(Req@, State)
			end;
		{end_of_part, Req@} ->
			if
				State#state.uploader /= undefined ->
					case file_uploader:done(State#state.uploader) of
						{ok, URL, Location} ->
							Files = [[
									  {name, FilenameB},
									  {url, URL}]|State#state.files],
							receive_file(Req@, State#state{ filename = undefined,
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
					receive_file(Req@, State)
			end;
		{eof, Req@} ->
			{ok, Req@} = cowboy_http_req:reply(200, 
											   [{<<"Content-Type">>, <<"application/json">>}],
											   jsx:to_json(State#state.files),
											   Req@),
			{Req@, State#state{ files = [] }}
	end.

