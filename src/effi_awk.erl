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
          run_extended_script/2] ).



-spec bind_singleton_boolean( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_boolean( _ArgName, _Value ) -> error( nyi ).


-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( _ArgName, _Value ) -> error( nyi ).


-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( _ArgName, _Value ) -> error( nyi ).


-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( _ArgName, _Value ) -> error( nyi ).


-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName ) -> error( invalid_op ).


-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) -> error( invalid_op ).


-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName ) -> error( invalid_op ).


-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName ) -> error( invalid_op ).

-spec prefix() ->
  binary().

prefix() -> <<>>.


-callback end_of_transmission() ->
  binary().

-callback suffix() ->
  binary().

-callback process_script( Script :: binary() ) ->
  binary().

% Returns either ok or error followed by standard/error output binary. If the
% run was successful also the return binding list is packaged in the return
% value.
-callback run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.