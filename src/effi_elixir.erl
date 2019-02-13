%% -*- erlang -*-
%%
%% Effi: Erlang foreign function interface
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @version 0.1.7
%% @copyright 2015-2019
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( effi_elixir ).
-behaviour( effi ).

-export( [bind_singleton_boolean/2, bind_singleton_string/2,
          bind_boolean_list/2, bind_string_list/2,
          prefix/0, suffix/0, end_of_transmission/0, process_script/1,
          run_extended_script/2, echo_singleton_boolean/1,
          echo_singleton_string/1, echo_boolean_list/1,
          echo_string_list/1] ).

-include( "effi.hrl" ).

-spec run_extended_script( ExtendedScript, Dir ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string().

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.exs"], "/" ),
  Call = "elixir __script.exs",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


-spec bind_singleton_boolean( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_boolean( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->
  <<ArgName/binary, " = ", Value/binary, "\n">>.


-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( ArgName, Value ) ->
  <<ArgName/binary, " = \"", Value/binary, "\"\n">>.


-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( ArgName, Value ) ->
  S = string:join( [binary_to_list( V ) || V <- Value], ", " ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "],\n">>.


-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( ArgName, Value ) ->
  S = string:join( ["\""++binary_to_list( V )++"\"" || V <- Value], ", " ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "]\n">>.


-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName ) ->
  <<":io.format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"~p\\\"}~n\", [", ArgName/binary, "] )\n">>.


-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) ->
  <<":io.format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"~s\\\"}~n\", [", ArgName/binary, "] )\n">>.


-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName ) ->
  <<":io.format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":[~s]}~n\", [Enum.join( ( for v <- ",
    ArgName/binary, ", do: if v, do: \"\\\"true\\\"\", else: \"\\\"false\\\"\" ), \", \" )] )\n">>.

-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName ) ->
  <<":io.format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":[~s]}~n\", [Enum.join( ( for v <- ",
    ArgName/binary, ", do: \"\\\"#{v}\\\"\" ), \", \" )] )\n">>.


-spec prefix() ->
  binary().

prefix() ->
  <<>>.


-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  <<"IO.puts( \"", ?EOT, "\\n\" )\n">>.


-spec suffix() ->
  binary().

suffix() ->
  <<>>.


-spec process_script( Script :: binary() ) ->
  binary().

process_script( Script ) ->
 Script.