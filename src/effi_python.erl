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

-module( effi_python ).
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
          run_extended_script/2] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "effi.hrl" ).

%%====================================================================
%% Effi callback function implementations
%%====================================================================



-spec run_extended_script( ExtendedScript, Dir ) ->
          {ok, binary(), [#{ atom() => _ }]}
        | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string().

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.py"], "/" ),
  Call = "python __script.py",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


bind_singleton_boolean( ArgName, <<"true">> ) ->
  <<ArgName/binary, " = True\n">>;

bind_singleton_boolean( ArgName, <<"false">> ) ->
  <<ArgName/binary, " = False\n">>.



-spec bind_singleton_string( ArgName, Value ) -> binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->

  <<ArgName/binary, " = '", Value/binary, "'\n">>.

bind_boolean_list( ArgName, Value ) ->

  F =
    fun
      ( <<"true">> )  -> "True";
      ( <<"false">> ) -> "False"
    end,

  S = string:join( [F( V ) || V <- Value], "," ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "]\n">>.

bind_string_list( ArgName, Value ) ->
  S = string:join( ["'"++binary_to_list( V )++"'" || V <- Value], "," ),
  B = list_to_binary( S ),
  <<ArgName/binary, " = [", B/binary, "]\n">>.


echo_singleton_boolean( ArgName )
when is_binary( ArgName ) ->

  <<"if ", ArgName/binary, ":\n  print( '", ?MSG, "{\"arg_name\":\"",
    ArgName/binary, "\",\"value\":\"true\"}\\n' )\nelse:\n  print( '", ?MSG,
    "{\"arg_name\":\"", ArgName/binary, "\",\"value\":\"false\"}\\n' )\n">>.

-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"print( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":\"'+str( ", ArgName/binary, " )+'\"}\\n' )\n">>.


echo_boolean_list( ArgName ) ->
  <<"print( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":['+','.join( map( lambda x: '\"true\"' if x else '\"false\"', ",
    ArgName/binary, " ) )+']}\\n')\n">>.

-spec echo_string_list( ArgName :: binary() ) -> binary().

echo_string_list( ArgName )
when is_binary( ArgName ) ->
  <<"print( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":['+','.join( map( lambda x: '\"%s\"'%(x), ", ArgName/binary,
    " ) )+']}\\n')\n">>.

prefix() ->
  <<>>.

end_of_transmission() ->
  <<"print( '", ?EOT, "' )\n">>.

suffix() ->
  <<>>.

process_script( Script ) ->
  B1 = binary:replace( Script, <<$\r>>, <<"">>, [global] ),
  B2 = binary:replace( B1, <<$\n>>, <<"\n ">>, [global] ),
  <<"if True:\n ", B2/binary>>.
