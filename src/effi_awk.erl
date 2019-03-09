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

-module( effi_awk ).
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


-record( run_info, {src_file} ).

%%====================================================================
%% Effi callback functions
%%====================================================================

-spec run_extended_script( ExtendedScript, Dir, RunInfo ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}
when  ExtendedScript :: binary(),
      Dir            :: string(),
      RunInfo        :: #run_info{}.

run_extended_script( ExtendedScript, Dir, RunInfo )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  #run_info{ src_file  = SrcFile } = RunInfo,

  ScriptFile = string:join( [Dir, "__script.awk"], "/" ),
  Call = io_lib:format( "awk -f __script.awk ~s > __result", [SrcFile] ),

  ok = file:write_file( ScriptFile, ExtendedScript ),
  Port = effi:create_port( Call, Dir ),

  RetBind = #{ arg_name => <<"result">>,
               value    => <<"__result">> },

  case effi:listen_port( Port ) of
    {error, B}          -> {error, B};
    {ok, B, RetBindLst} -> {ok, B, [RetBind|RetBindLst]}
  end.


-spec get_run_info( Request :: #{ atom() => _ } ) -> #run_info{}.

get_run_info( Request ) ->
  #{ arg_bind_lst := ArgBindLst } = Request,
  [#{ value := Value}|_] = ArgBindLst,
  #run_info{ src_file = Value }.

bind_singleton_boolean( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->
  error( nyi ).


-spec bind_singleton_string( ArgName, Value ) -> binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->
  <<"BEGIN { ", ArgName/binary, " = \"", Value/binary, "\" }\n">>.

bind_boolean_list( _ArgName, _Value ) ->
  error( nyi ).

bind_string_list( ArgName, Value )
when is_binary( ArgName ),
     is_list( Value ) ->
  error( nyi ).
  
echo_singleton_boolean( _ArgName ) ->
  error( nyi ).

-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->
  error( nyi ).

echo_boolean_list( _ArgName ) ->
  error( nyi ).

echo_string_list( _ArgName ) ->
  error( nyi ).

prefix() ->
  <<>>.

end_of_transmission() ->
  <<"END { print \"", ?EOT, "\" }\n">>.

suffix() ->
  <<>>.

process_script( Script ) ->
  Script.