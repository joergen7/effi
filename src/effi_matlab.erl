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

-module( effi_matlab ).

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

bind_singleton_boolean( ArgName, Value ) ->
  effi_octave:bind_singleton_boolean( ArgName, Value ).

bind_singleton_string( Argname, Value ) ->
  effi_octave:bind_singleton_string( Argname, Value ).

bind_boolean_list( ArgName, Value ) ->
  effi_octave:bind_boolean_list( ArgName, Value ).

bind_string_list( ArgName, Value ) ->
  effi_octave:bind_string_list( ArgName, Value ).

echo_singleton_boolean( ArgName ) ->
  effi_octave:echo_singleton_boolean( ArgName ).

echo_singleton_string( ArgName ) ->
  effi_octave:echo_singleton_string( ArgName ).

echo_boolean_list( ArgName ) ->
  effi_octave:echo_boolean_list( ArgName ).

echo_string_list( ArgName ) ->
  effi_octave:echo_string_list( ArgName ).

prefix() ->
  effi_octave:prefix().

end_of_transmission() ->
  effi_octave:end_of_transmission().

suffix() ->
  Suffix = effi_octave:suffix(),
  <<Suffix/binary, "exit( 0 );\n">>.

process_script( Script ) ->
  effi_octave:process_script( Script ).


-spec run_extended_script( ExtendedScript, Dir, RunInfo ) ->
          {ok, binary(), [#{ atom() => _ }]}
        | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string(),
     RunInfo        :: _.

run_extended_script( ExtendedScript, Dir, _ )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "script.m"], "/" ),
  Call = "matlab -nodisplay -nojvm -r script",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).

-spec get_run_info( Request :: #{ atom() => _ } ) -> [].

get_run_info( _Request ) ->
  [].

