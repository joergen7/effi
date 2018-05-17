%% -*- erlang -*-
%%
%% effi
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
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( effi_java ).
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

-spec bind_singleton_boolean( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_boolean( ArgName, Value ) ->
  <<"final boolean ", ArgName/binary, " = ", Value/binary, ";\n">>.


-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( ArgName, Value ) ->
  <<"final String ", ArgName/binary, " = \"", Value/binary, "\";\n">>.


-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( ArgName, ValueLst ) ->
  

  S = string:join( [binary_to_list( Value ) || Value <- ValueLst], ", " ),
  B = list_to_binary( S ),

  <<"final boolean[] ", ArgName/binary, " = { ", B/binary, " };\n">>.

-spec bind_string_list( ArgName :: binary(), ValueLst :: [binary()] ) ->
  binary().

bind_string_list( ArgName, ValueLst )
when is_binary( ArgName ),
     is_list( ValueLst ) ->

  F =
    fun( X ) when is_binary( X ) ->
      "\""++binary_to_list( X )++"\""
    end,

  S = string:join( [F( Value ) || Value <- ValueLst], ", " ),
  B = list_to_binary( S ),

  <<"final String[] ", ArgName/binary, " = { ", B/binary, " };\n">>.


-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName ) ->
  <<"System.out.println( new StringBuffer().append( \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": \\\"\" ).append( ", ArgName/binary, " ).append( \"\\\" }\") );\n">>.

-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) ->
  <<"System.out.println( new StringBuffer().append( \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": \\\"\" ).append( ", ArgName/binary, " ).append( \"\\\" }\" ) );\n">>.


-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName ) ->
  <<"StringBuffer __s = new StringBuffer();\n",
    "__s.append( \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": [\" );\n",
    "boolean __comma = false;\n"
    "for( boolean x : ", ArgName/binary, " ) {\n",
    "  if( __comma )\n",
    "    __s.append( ',' );\n"
    "  __comma = true;"
    "  __s.append( '\"' ).append( x ).append( '\"' );\n",
    "}\n",
    "System.out.println( __s.append( \"] }\" ) );\n\n">>.



-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName ) ->
  <<"StringBuffer __s = new StringBuffer();\n",
    "__s.append( \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": [\" );\n",
    "boolean __comma = false;\n"
    "for( String x : ", ArgName/binary, " ) {\n",
    "  if( __comma )\n",
    "    __s.append( ',' );\n"
    "  __comma = true;"
    "  __s.append( '\"' ).append( x ).append( '\"' );\n",
    "}\n",
    "System.out.println( __s.append( \"] }\" ) );\n\n">>.


-spec prefix() ->
  binary().

prefix() ->
  <<"import java.io.BufferedReader;\n",
    "import java.io.BufferedWriter;\n",
    "import java.io.FileReader;\n",
    "import java.io.FileWriter;\n",
    "import java.io.File;\n",
    "import java.io.IOException;\n",
    "public class Main {\n",
    "public static void main( String[] __args ) throws IOException {\n">>.


-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  <<"System.out.println( \"", ?EOT, "\" );\n">>.


-spec suffix() ->
  binary().

suffix() ->
  <<"}\n}\n">>.


-spec process_script( Script :: binary() ) ->
  binary().

process_script( Script ) ->
  Script.


-spec run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir ) ->

  JavaFile = string:join( [Dir, "Main.java"], "/" ),
  ok = file:write_file( JavaFile, ExtendedScript ),

  ScriptFile = string:join( [Dir, "__script.sh"], "/" ),
  ok = file:write_file( ScriptFile, "set -eu -o pipefail\njavac Main.java\njava Main\n" ),

  Call = "bash __script.sh",


  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).