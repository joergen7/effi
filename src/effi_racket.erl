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

-module( effi_racket ).
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

bind_singleton_boolean( ArgName, <<"true">> ) ->
  <<"(define ", ArgName/binary, " #t)\n">>;

bind_singleton_boolean( ArgName, <<"false">> ) ->
  <<"(define ", ArgName/binary, " #f)\n">>.


-spec bind_singleton_string( ArgName :: binary(), Value :: binary() ) ->
  binary().

bind_singleton_string( ArgName, Value ) ->
  <<"(define ", ArgName/binary, " \"", Value/binary, "\")\n">>.


-spec bind_boolean_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_boolean_list( ArgName, ValueLst ) ->
  
  F =
    fun
      ( <<"true">>, Acc )  -> <<Acc/binary, " #t">>;
      ( <<"false">>, Acc ) -> <<Acc/binary, " #f">>
    end,

  B = lists:foldl( F, <<>>, ValueLst ),

  <<"(define ", ArgName/binary, " (list", B/binary, "))\n">>.

-spec bind_string_list( ArgName :: binary(), Value :: [binary()] ) ->
  binary().

bind_string_list( ArgName, ValueLst ) ->

  F =
    fun( X, Acc ) ->
      <<Acc/binary, " \"", X/binary, "\"">>
    end,

  B = lists:foldl( F, <<>>, ValueLst ),

  <<"(define ", ArgName/binary, " (list", B/binary, "))\n">>.


-spec echo_singleton_boolean( ArgName :: binary() ) ->
  binary().

echo_singleton_boolean( ArgName ) ->
  <<"(if ", ArgName/binary, "\n",
    "  (printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":\\\"true\\\"}\\n\")\n",
    "  (printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":\\\"false\\\"}\\n\"))\n">>.

-spec echo_singleton_string( ArgName :: binary() ) ->
  binary().

echo_singleton_string( ArgName ) ->
  <<"(printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":~s}\\n\" ", ArgName/binary, ")\n">>.


-spec echo_boolean_list( ArgName :: binary() ) ->
  binary().

echo_boolean_list( ArgName ) ->
  <<"(let* ([boolean-to-string (lambda (x) (if x \"\\\"true\\\"\" \"\\\"false\\\"\"))]\n",
    "       [l                 (map boolean-to-string ", ArgName/binary, ")]\n",
    "       [s                 (string-join l \",\")])\n",
    "  (printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":[~a]}\\n\" s))\n">>.



-spec echo_string_list( ArgName :: binary() ) ->
  binary().

echo_string_list( ArgName ) ->
  <<"(let* ([quote-string (lambda (x) (string-append \"\\\"\" x \"\\\"\"))]\n",
    "       [l            (map quote-string ", ArgName/binary, ")]\n",
    "       [s            (string-join l \",\")])\n",
    "  (printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":[~a]}\\n\" s))\n">>.


-spec prefix() ->
  binary().

prefix() ->
  <<"#lang racket/base\n">>.


-spec end_of_transmission() ->
  binary().

end_of_transmission() ->
  <<"(printf \"", ?EOT, "\\n\")\n">>.


-spec suffix() ->
  binary().

suffix() ->
  <<>>.


-spec process_script( Script :: binary() ) ->
  binary().

process_script( Script ) ->
  Script.


-spec run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir ) ->

  ScriptFile = string:join( [Dir, "__script.rkt"], "/" ),
  Call = "racket __script.rkt",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).