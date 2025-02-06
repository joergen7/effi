%% -*- erlang -*-
%%
%% Effi: Erlang foreign function interface
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc The standalone application entry point is {@link main/1}.
%% The create_port callback defined here is an abstract way to execute child
%% processes in foreign languages.
%% There are two foreign language interfaces, both implementing this callback,
%% {@link effi_script} (e.g., Perl, Python) and {@link effi_interact} (e.g.,
%% Bash, R).
%%
%% @end
%% -------------------------------------------------------------------

-module(effi_octave).

-behaviour(effi).

%%====================================================================
%% Exports
%%====================================================================

% effi callbacks
-export([bind_singleton_boolean/2,
         bind_singleton_string/2,
         bind_boolean_list/2,
         bind_string_list/2,
         echo_singleton_boolean/1,
         echo_singleton_string/1,
         echo_boolean_list/1,
         echo_string_list/1,
         prefix/0,
         suffix/0,
         end_of_transmission/0,
         process_script/1,
         run_extended_script/3,
         get_run_info/1]).

%%====================================================================
%% Includes
%%====================================================================

-include("effi.hrl").

%%====================================================================
%% Effi callback function implementations
%%====================================================================


-spec run_extended_script(ExtendedScript, Dir, RunInfo) ->
          {ok, binary(), [#{atom() => binary()}]} |
          {error, binary()}
              when ExtendedScript :: binary(),
                   Dir :: string(),
                   RunInfo :: _.

run_extended_script(ExtendedScript, Dir, _)
  when is_binary(ExtendedScript),
       is_list(Dir) ->

    ScriptFile = string:join([Dir, "__script.m"], "/"),
    Call = "octave __script.m",

    ok = file:write_file(ScriptFile, ExtendedScript),

    Port = effi:create_port(Call, Dir),

    effi:listen_port(Port).


-spec bind_singleton_string(ArgName, Value) -> <<_:56, _:_*8>>
              when ArgName :: binary(),
                   Value :: binary().

bind_singleton_string(ArgName, Value)
  when is_binary(ArgName),
       is_binary(Value) ->

    <<ArgName/binary, " = '", Value/binary, "';\n">>.


-spec bind_singleton_boolean(ArgName, Value) -> <<_:64, _:_*8>>
              when ArgName :: binary(),
                   Value :: <<_:32, _:_*8>>.

bind_singleton_boolean(ArgName, <<"true">>) ->
    <<ArgName/binary, " = true;\n">>;

bind_singleton_boolean(ArgName, <<"false">>) ->
    <<ArgName/binary, " = false;\n">>.


-spec bind_boolean_list(ArgName, Value) -> binary()
              when ArgName :: binary(),
                   Value :: [binary()].

bind_boolean_list(ArgName, Value)
  when is_binary(ArgName),
       is_list(Value) ->

    StrLst = lists:map(fun binary_to_list/1, Value),
    B = list_to_binary("{" ++ string:join(StrLst, ", ") ++ "}"),
    <<ArgName/binary, " = ", B/binary, ";\n">>.


-spec bind_string_list(ArgName, Value) -> <<_:40, _:_*8>>
              when ArgName :: binary(),
                   Value :: [binary()].

bind_string_list(ArgName, Value)
  when is_binary(ArgName),
       is_list(Value) ->

    StrLst = [ "'" ++ binary_to_list(V) ++ "'" || V <- Value ],
    B = list_to_binary("{" ++ string:join(StrLst, ", ") ++ "}"),
    <<ArgName/binary, " = ", B/binary, ";\n">>.


-spec echo_singleton_string(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_singleton_string(ArgName)
  when is_binary(ArgName) ->

    <<"if ~ischar( ", ArgName/binary, " )\n",
      "  error( '", ArgName/binary, " not a string' )\n",
      "end\n",
      "display( ['", ?MSG, "{\"arg_name\":\"", ArgName/binary,
      "\",\"value\":\"', ", ArgName/binary, ", '\"}\\n'] )\n\n">>.


echo_singleton_boolean(ArgName)
  when is_binary(ArgName) ->

    <<"if ~islogical( ", ArgName/binary, " )\n",
      "  error( '", ArgName/binary, " not a logical' )\n",
      "end\n",
      "if ", ArgName/binary, "\n",
      "  display( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
      "\",\"value\":\"true\"}\\n' )\n",
      "else\n",
      "  display( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
      "\",\"value\":\"false\"}\\n' )\n",
      "end\n\n">>.


-spec echo_string_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_string_list(ArgName)
  when is_binary(ArgName) ->

    <<"if ~iscell( ", ArgName/binary, " )\n",
      "  error( '", ArgName/binary, " not a cell' )\n",
      "end\n",
      "for i = 1:prod( size( ", ArgName/binary, " ) )\n",
      "  if ~ischar( ", ArgName/binary, "{ i } )\n",
      "    error( '", ArgName/binary, " contains non-string elements' )\n",
      "  end\n",
      "end\n",
      "fprintf( 1, '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
      "\",\"value\":[' )\n",
      "for i = 1:prod( size( ", ArgName/binary, " ) )\n",
      "  if i ~= 1\n",
      "    fprintf( 1, ',' )\n",
      "  end\n",
      "  fprintf( 1, '\"%s\"', ", ArgName/binary, "{ i } )\n",
      "end\n",
      "fprintf( 1, ']}\\n' )\n\n">>.


-spec echo_boolean_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_boolean_list(ArgName) ->

    <<"if ~iscell( ", ArgName/binary, " )\n",
      "  error( '", ArgName/binary, " not a cell' )\n",
      "end\n",
      "for i = 1:prod( size( ", ArgName/binary, " ) )\n",
      "  if ~islogical( ", ArgName/binary, "{ i } )\n",
      "    error( '", ArgName/binary, " contains non-logical elements' )\n",
      "  end\n",
      "end\n",
      "fprintf( 1, '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
      "\",\"value\":[' )\n",
      "for i = 1:prod( size( ", ArgName/binary, " ) )\n",
      "  if i ~= 1\n",
      "    fprintf( 1, ',' )\n",
      "  end\n",
      "  if ", ArgName/binary, "\n",
      "    fprintf( 1, '\"true\"' )\n",
      "  else\n",
      "    fprintf( 1, '\"false\"' )\n",
      "  end\n",
      "end\n",
      "fprintf( 1, ']}\\n' )\n\n">>.


end_of_transmission() ->
    <<"display( '", ?EOT, "' )\n">>.


prefix() ->
    <<"try\n">>.


suffix() ->
    <<"catch e\n  display( e );\n  exit( -1 );\nend\n">>.


process_script(Script) ->
    Script.


-spec get_run_info(Request :: #{atom() => _}) -> [].

get_run_info(_Request) ->
    [].
