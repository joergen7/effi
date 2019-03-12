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

-module( effi_gnuplot ).
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
%% Effi callback functions
%%====================================================================

-spec bind_singleton_boolean( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_boolean( _ArgName, _Value ) ->
  error( nyi ).

-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( _ArgName, _Value ) ->
  error( nyi ).

-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( _ArgName, _Value ) ->
  error( nyi ).

-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( ArgName, Value ) ->
  L = binary:encode_unsigned( length( Value ) ),
  SLst = ["\""++binary_to_list( V )++"\"" || V <- Value],
  B = list_to_binary( string:join( SLst, "," ) ),
  <<"array ", ArgName/binary, "[", L/binary, "]=[", B/binary, "]\n">>.

-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( _ArgName ) ->
  error( nyi ).

-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) ->
  <<"print \"", ?MSG, "\", \"{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\", \\\"value\\\":\\\"\",", ArgName/binary, ",\"\\\"}\"">>.

-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( _ArgName ) ->
  error( nyi ).

-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( _ArgName ) ->
  error( nyi ).

-spec prefix() ->
  binary().

prefix() ->
  error( nyi ).

-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  error( nyi ).

-spec suffix() ->
  binary().

suffix() ->
  error( nyi ).

-spec process_script( Script :: binary() ) ->
  binary().

process_script( _Script ) ->
  error( nyi ).

-spec run_extended_script( ExtendedScript :: binary(),
                               Dir            :: string(),
                               RunInfo        :: _ ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( _ExtendedScript, _Dir, _RunInfo ) ->
  error( nyi ).

-spec get_run_info( Request :: #{ atom() => _ } ) -> _.

get_run_info( _Request ) ->
  error( nyi ).