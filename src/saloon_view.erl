%%%
%%% saloon_view: tools that deal with views.
%%%
%%% DTL extension is assumed to be ".dtl",
%%% Default template directory assumed to be "site/priv/templates".
%%% You may override the template directory by using get_m/2.
%%% Please use only lowercase ASCII characters in the template names.
%%% Not that it's required, just for the sake of good style. :)
%%% Please note that those functions are to only be used from your _view 
%%% modules and user input MUST NOT reach get_m. If it is happening, then
%%% you are doing something that is terribly wrong as list_to_atom is used 
%%% here.
%%%

-module(saloon_view).

-export([
		 get_m/1
		,get_m/2
	 ]).

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Exports
%%%

%% Get the module located under the default module path
-spec get_m(file:name()) -> atom().
get_m(TemplateName) ->
	get_m("site/priv/templates/", TemplateName).

%% Get the module located under a custom module path
-spec get_m(file:name(), file:name()) -> atom().
get_m(TemplatePath, TemplateName) ->
	TemplateLocation = TemplatePath ++ TemplateName,
	TemplateModuleMaybe = case init:get_argument(devmode) of
		{ok, _} -> 
			compile_template(TemplateLocation);
		_ ->
			ensure_template_file_loaded(TemplateLocation)
	end,
	case TemplateModuleMaybe of
		{error, Err} ->
			?debugFmt("Template compilation failed:~n~p", [{error, Err}]),
			error(recompile_error);
		TemplateModule ->
			TemplateModule
	end.

%%%
%%% Private functions
%%%
-spec compile_template(file:name()) -> atom().
compile_template(TemplateLocation) ->
	TemplateModule = list_to_atom(filename:basename(TemplateLocation, ".dtl")),
	CompilationResult = erlydtl:compile(
		TemplateLocation,
		TemplateModule,
		[{out_dir, "site/ebin"}, {custom_tags_modules, [saloon_lang]}]
	),
	case CompilationResult of
		ok  -> TemplateModule;
		Err -> Err
	end.

-spec ensure_template_file_loaded(file:name()) -> atom() | boolean().
ensure_template_file_loaded(TemplateLocation) ->
	TemplateModule = list_to_atom(filename:basename(TemplateLocation, ".dtl")),
	case code:is_loaded(TemplateModule) of
		{file, _} -> TemplateModule;
		false     -> compile_template(TemplateLocation) 
	end.
