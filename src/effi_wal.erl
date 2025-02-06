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

-module(effi_wal).
-behaviour(effi).

%%====================================================================
%% Exports
%%====================================================================

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


-spec bind_singleton_boolean(ArgName :: binary(), Value :: <<_:32, _:_*8>>) -> <<_:64, _:_*8>>.

bind_singleton_boolean(ArgName, <<"true">>) when is_binary(ArgName) ->
    <<"(set (", ArgName/binary, " #t))\n">>;

bind_singleton_boolean(ArgName, <<"false">>) when is_binary(ArgName) ->
    <<"(set (", ArgName/binary, " #f))\n">>.


-spec bind_singleton_string(ArgName :: binary(), Value :: binary()) -> <<_:64, _:_*8>>.

bind_singleton_string(ArgName, Value) when is_binary(ArgName), is_binary(Value) ->
    <<"(set (", ArgName/binary, " \"", Value/binary, "\"))\n">>.


-spec bind_boolean_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_boolean_list(ArgName, Value) when is_binary(ArgName), is_list(Value) ->

    F =
        fun(<<"true">>, Acc) -> <<Acc/binary, " #t">>;
           (<<"false">>, Acc) -> <<Acc/binary, " #f">>
        end,

    B = lists:foldl(F, <<>>, Value),

    <<"(set (", ArgName/binary, " (list", B/binary, ")))\n">>.


-spec bind_string_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_string_list(ArgName, Value) ->
    F = fun(S, Acc) -> <<Acc/binary, " \"", S/binary, "\"">> end,
    B = lists:foldl(F, <<>>, Value),
    <<"(set (", ArgName/binary, " (list", B/binary, ")))\n">>.


-spec echo_singleton_boolean(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_singleton_boolean(ArgName) when is_binary(ArgName) ->
    <<"(printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":\\\"%s\\\"}\\n\" (if ", ArgName/binary, " \"true\" \"false\"))\n">>.


-spec echo_singleton_string(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_singleton_string(ArgName) ->
    <<"(printf \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":\\\"%s\\\"}\\n\" ", ArgName/binary, ")\n">>.


-spec echo_boolean_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_boolean_list(ArgName) ->
    <<"(let ((f (lambda (x) (if x \"true\" \"false\"))))\n  (printf \"<MSG>{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":[\")\n  (if (length ", ArgName/binary, ")\n     (let ((hd (first ", ArgName/binary, ")) (tl (rest ", ArgName/binary, "))) (printf \"\\\"%s\\\"\" (f hd)) (map (lambda (x) (printf \",\\\"%s\\\"\" (f x))) tl)))\n  (printf \"]}\\n\"))\n">>.


-spec echo_string_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_string_list(ArgName) ->
    <<"(printf \"<MSG>{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":[\")\n(if (length ", ArgName/binary, ")\n   (let ((hd (first ", ArgName/binary, ")) (tl (rest ", ArgName/binary, "))) (printf \"\\\"%s\\\"\" hd) (map (lambda (x) (printf \",\\\"%s\\\"\" x)) tl)))\n(printf \"]}\\n\")\n">>.


-spec prefix() -> <<>>.

prefix() ->
    <<>>.


-spec end_of_transmission() -> <<_:128>>.

end_of_transmission() ->
    <<"(print \"", ?EOT, "\")\n">>.


-spec suffix() -> <<>>.

suffix() ->
    <<>>.


-spec process_script(Script :: binary()) ->
          binary().

process_script(Script) when is_binary(Script) ->
    Script.


-spec run_extended_script(ExtendedScript :: binary(),
                          Dir :: string(),
                          RunInfo :: _) ->
          {ok, binary(), [#{atom() => binary()}]} |
          {error, binary()}.

run_extended_script(ExtendedScript, Dir, _RunInfo) ->

    ScriptFile = string:join([Dir, "__script.wal"], "/"),
    Call = "wal __script.wal",

    ok = file:write_file(ScriptFile, ExtendedScript),
    Port = effi:create_port(Call, Dir),

    effi:listen_port(Port).


-spec get_run_info(Request :: #{atom() => _}) -> _.

get_run_info(_Request) ->
    [].
