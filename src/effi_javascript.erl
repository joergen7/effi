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

-module( effi_javascript ).
-behaviour( effi ).

-export( [bind_singleton_boolean/2, bind_singleton_string/2,
          bind_boolean_list/2, bind_string_list/2,
          prefix/0, suffix/0, end_of_transmission/0, process_script/1,
          run_extended_script/3, echo_singleton_boolean/1,
          echo_singleton_string/1, echo_boolean_list/1,
          echo_string_list/1,
          get_run_info/1] ).

-include( "effi.hrl" ).


-spec run_extended_script( ExtendedScript, Dir, RunInfo ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string(),
     RunInfo        :: _.

run_extended_script( ExtendedScript, Dir, _ )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.js"], "/" ),
  Call = "node __script.js",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


-spec bind_singleton_boolean( ArgName, Value ) ->
  binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_boolean( ArgName, <<"true">> )
when is_binary( ArgName ) ->
  <<"var ", ArgName/binary, " = true;\n">>;

bind_singleton_boolean( ArgName, <<"false">> )
when is_binary( ArgName ) ->
  <<"var ", ArgName/binary, " = false;\n">>.


-spec bind_singleton_string( ArgName, Value ) ->
  binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->
  <<"var ", ArgName/binary, " = '", Value/binary, "';\n">>.


-spec bind_boolean_list( ArgName, Value ) ->
  binary()
when ArgName :: binary(),
     Value   :: [binary()].

bind_boolean_list( ArgName, Value )
when is_binary( ArgName ),
     is_list( Value ) ->
  SLst = [binary_to_list( V ) || V <- Value],
  B = list_to_binary( string:join( SLst, ", " ) ),
  <<"var ", ArgName/binary, " = [ ", B/binary, " ];\n">>.


-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( ArgName, Value )
when is_binary( ArgName ),
     is_list( Value ) ->
  SLst = ["'"++binary_to_list( V )++"'" || V <- Value],
  B = list_to_binary( string:join( SLst, ", " ) ),
  <<"var ", ArgName/binary, " = [ ", B/binary, " ];\n">>.


-spec prefix() ->
  binary().

prefix() -> <<>>.


-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  <<"console.log( '", ?EOT, "' );\n">>.


-spec suffix() ->
  binary().

suffix() -> <<>>.


-spec process_script( Script :: binary() ) ->
  binary().

process_script( Script )
when is_binary( Script ) ->
  Script.


-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName )
when is_binary( ArgName ) ->
  <<"console.log( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":\"'+String( ", ArgName/binary, " )+'\"}' );\n">>.


-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->
  <<"console.log( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":\"'+", ArgName/binary, "+'\"}' );\n">>.


-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName )
when is_binary( ArgName ) ->
  <<"console.log( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":['+String( ", ArgName/binary,
    ".map( x => '\"'+String( x )+'\"' ) )+']}' );\n">>.

-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName )
when is_binary( ArgName ) ->
  <<"console.log( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":['+String( ", ArgName/binary,
    ".map( x => '\"'+x+'\"' ) )+']}' );\n">>.

-spec get_run_info( Request :: #{ atom() => _ } ) -> [].

get_run_info( _Request ) ->
  [].
