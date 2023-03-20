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
%%
%% @end
%% -------------------------------------------------------------------

-module( effi_erlang ).
-behaviour( effi ).

%%====================================================================
%% Exports
%%====================================================================

% effi callbacks
-export( [bind_singleton_boolean/2,
          bind_singleton_string/2,
          bind_boolean_list/2,
          bind_string_list/2,
          echo_singleton_boolean/1,
          echo_singleton_string/1,
          echo_boolean_list/1,
          echo_string_list/1,
          prefix/0,
          end_of_transmission/0,
          suffix/0,
          process_script/1,
          run_extended_script/3,
          get_run_info/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "effi.hrl" ).


%%====================================================================
%% Effi callback function implementations
%%====================================================================

-spec bind_singleton_boolean( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_boolean( ArgName, Value ) ->
  <<ArgName/binary, " = ", Value/binary, ",\n">>.


-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( ArgName, Value ) ->
  <<ArgName/binary, " = \"", Value/binary, "\",\n">>.

-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( ArgName, ValueLst ) ->
  S = string:join( [binary_to_list( Value ) || Value <- ValueLst], ", " ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "],\n">>.

-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( ArgName, ValueLst ) ->
  S = string:join( ["\""++binary_to_list( Value )++"\"" || Value <- ValueLst], ", " ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "],\n">>.

-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName ) ->
  <<"io:format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"~p\\\"}~n\", [", ArgName/binary, "] ),\n">>.

-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) ->
  <<"io:format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"~s\\\"}~n\", [", ArgName/binary, "] ),\n">>.

-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName ) ->
  <<"io:format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":[~s]}~n\", [string:join( [if V -> \"\\\"true\\\"\"; true -> \"\\\"false\\\"\" end || V <- ",
    ArgName/binary, "], \", \" )] ),\n">>.

-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName ) ->
  <<"io:format( \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":[~s]}~n\", [string:join( [\"\\\"\"++V++\"\\\"\" || V <- ",
    ArgName/binary, "], \", \" )] ),\n">>.

-spec prefix() ->
  binary().

prefix() ->
  <<"-module( __script ).\n-export( [main/1] ).\n\nmain( _ ) ->\n\n">>.


-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  <<"io:format( \"", ?EOT, "\\n\" ),\n">>.


-spec suffix() ->
  binary().

suffix() ->
  <<"ok.\n">>.


-spec process_script( Script :: binary() ) ->
  binary().

process_script( Script ) ->
  Script1 = string:trim( Script, trailing ),
  Skip = byte_size( Script1 )-1,
  case Script1 of
    <<X:Skip/binary, ".">> -> <<X/binary, ",\n">>;
    X                      -> <<X/binary, ",\n">>
  end.


-spec run_extended_script( ExtendedScript :: binary(), Dir :: string(), RunInfo :: _ ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir, _ ) ->

  ScriptFile = string:join( [Dir, "__script.erl"], "/" ),
  Call = "escript __script.erl",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).

-spec get_run_info( Request :: #{ atom() => _ } ) -> [].

get_run_info( _Request ) ->
  [].
