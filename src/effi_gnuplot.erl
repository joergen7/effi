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

-module(effi_gnuplot).
-behaviour(effi).

% effi callbacks
-export([bind_singleton_boolean/2,
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
         get_run_info/1]).

-include("effi.hrl").

%%====================================================================
%% Effi callback functions
%%====================================================================


-spec bind_singleton_boolean(ArgName :: binary(), Value :: binary()) ->
          binary().

bind_singleton_boolean(ArgName, Value) ->
    V = case Value of
            <<"true">> -> <<"1">>;
            <<"false">> -> <<"0">>
        end,
    <<ArgName/binary, "=", V/binary, "\n">>.


-spec bind_singleton_string(ArgName :: binary(), Value :: binary()) ->
          binary().

bind_singleton_string(ArgName, Value) ->
    <<ArgName/binary, "=\"", Value/binary, "\"\n">>.


-spec bind_boolean_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_boolean_list(ArgName, Value) ->
    F = fun(<<"true">>) -> "1";
           (<<"false">>) -> "0"
        end,
    L = list_to_binary(integer_to_list(length(Value))),
    SLst = [ F(V) || V <- Value ],
    B = list_to_binary(string:join(SLst, ",")),
    <<"array ", ArgName/binary, "[", L/binary, "]=[", B/binary, "]\n">>.


-spec bind_string_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_string_list(ArgName, Value) ->
    L = list_to_binary(integer_to_list(length(Value))),
    SLst = [ "\"" ++ binary_to_list(V) ++ "\"" || V <- Value ],
    B = list_to_binary(string:join(SLst, ",")),
    <<"array ", ArgName/binary, "[", L/binary, "]=[", B/binary, "]\n">>.


-spec echo_singleton_boolean(ArgName :: binary()) ->
          binary().

echo_singleton_boolean(ArgName) ->
    <<"if (", ArgName/binary, ") {print \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
      "\\\",\\\"value\\\":\\\"true\\\"}\"} else {print \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
      "\\\",\\\"value\\\":\\\"false\\\"}\"}">>.


-spec echo_singleton_string(ArgName :: binary()) ->
          binary().

echo_singleton_string(ArgName) ->
    <<"print \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
      "\\\",\\\"value\\\":\\\"\".", ArgName/binary, ".\"\\\"}\"\n">>.


-spec echo_boolean_list(ArgName :: binary()) ->
          binary().

echo_boolean_list(_ArgName) ->
    error(nyi).


-spec echo_string_list(ArgName :: binary()) ->
          binary().

echo_string_list(_ArgName) ->
    error(nyi).


-spec prefix() ->
          binary().

prefix() ->
    <<>>.


-spec end_of_transmission() ->
          binary().

end_of_transmission() ->
    <<"print \"", ?EOT, "\"\n">>.


-spec suffix() ->
          binary().

suffix() ->
    <<>>.


-spec process_script(Script :: binary()) ->
          binary().

process_script(Script) ->
    Script.


-spec get_run_info(Request :: #{atom() => _}) -> [].

get_run_info(_Request) ->
    [].


-spec run_extended_script(ExtendedScript :: binary(),
                          Dir :: string(),
                          RunInfo :: []) ->
          {ok, binary(), [#{atom() => binary()}]} |
          {error, binary()}.

run_extended_script(ExtendedScript, Dir, _RunInfo) ->

    ScriptFile = string:join([Dir, "__script"], "/"),
    Call = "gnuplot __script",

    ok = file:write_file(ScriptFile, ExtendedScript),

    Port = effi:create_port(Call, Dir),

    effi:listen_port(Port).
