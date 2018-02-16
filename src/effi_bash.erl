%% -*- erlang -*-
%%
%% Effi: Erlang Foreign Function Interface
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.4
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%% @end
%% -------------------------------------------------------------------


-module( effi_bash ).
-behaviour( effi ).

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
          run_extended_script/2] ).

-include( "effi.hrl" ).






%%====================================================================
%% Effi callback function implementations
%%====================================================================


-spec run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.sh"], "/" ),
  Call = "bash __script.sh",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


bind_singleton_boolean( _ArgName, _Value ) ->
  error( nyi ).


-spec bind_singleton_string( ArgName, Value ) -> binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->

  <<ArgName/binary, "='", Value/binary, "'\n">>.

bind_boolean_list( _ArgName, _Value ) ->
  error( nyi ).

bind_string_list( _ArgName, _Value ) ->
  error( nyi ).


echo_singleton_boolean( _ArgName ) ->
  error( nyi ).

-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"echo \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"$", ArgName/binary, "\\\"}\"\n">>.

echo_boolean_list( _ArgName ) ->
  error( nyi ).

echo_string_list( _ArgName ) ->
  error( nyi ).

prefix() ->
  <<"set -eu -o pipefail\n">>.

end_of_transmission() ->
  <<"echo '", ?EOT, "'\n">>.

suffix() ->
  <<>>.

process_script( Script ) ->
  Script.