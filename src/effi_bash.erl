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
          run_extended_script/3,
          get_run_info/1] ).

-include( "effi.hrl" ).






%%====================================================================
%% Effi callback function implementations
%%====================================================================


-spec run_extended_script( ExtendedScript :: binary(), Dir :: string(), RunInfo :: _ ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir, _ )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.sh"], "/" ),
  Call = "bash __script.sh",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


bind_singleton_boolean( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->
  <<ArgName/binary, "='", Value/binary, "'\n">>.


-spec bind_singleton_string( ArgName, Value ) -> binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->

  <<ArgName/binary, "='", Value/binary, "'\n">>.

bind_boolean_list( ArgName, Value ) ->
  bind_string_list( ArgName, Value ).

bind_string_list( ArgName, Value )
when is_binary( ArgName ),
     is_list( Value ) ->
  SLst = ["'"++binary_to_list( V )++"'" || V <- Value],
  S = string:join( SLst, " " ),
  B = list_to_binary( S ),
  <<ArgName/binary, "=(", B/binary, ")\n">>.
  


echo_singleton_boolean( ArgName ) ->
  <<"if [ $", ArgName/binary, " == 'true' ]\n",
    "then\n",
    "  echo '", ?MSG, "{\"arg_name\":\"", ArgName/binary, "\",\"value\":\"true\"}'\n",
    "else\n",
    "  echo '", ?MSG, "{\"arg_name\":\"", ArgName/binary, "\",\"value\":\"false\"}'\n",
    "fi\n\n">>.

-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"echo \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"$", ArgName/binary, "\\\"}\"\n">>.

echo_boolean_list( ArgName ) ->
  B = echo_string_list( ArgName ),
  <<"for x in ${", ArgName/binary, "[@]}\n",
    "do\n",
    "  if [ $x != 'true' ]\n",
    "  then\n",
    "    if [ $x != 'false' ]\n",
    "    then\n",
    "      echo non-Boolean value in ", ArgName/binary, "\n",
    "      exit -1\n",
    "    fi\n",
    "  fi\n",
    "done\n",
    B/binary>>.

echo_string_list( ArgName ) ->
  <<"TMP=`printf \",\\\"%s\\\"\" ${", ArgName/binary, "[@]}`\n",
    "TMP=${TMP:1}\n",
    "echo \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":[$TMP]}\"\n\n">>.

prefix() ->
  <<"set -eu -o pipefail\n">>.

end_of_transmission() ->
  <<"echo '", ?EOT, "'\n">>.

suffix() ->
  <<>>.

process_script( Script ) ->
  Script.

-spec get_run_info( Request :: #{ atom() => _ } ) -> [].

get_run_info( _Request ) ->
  [].
