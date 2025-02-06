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

-module(effi_perl).
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

%%====================================================================
%% Includes
%%====================================================================

-include("effi.hrl").

%%====================================================================
%% Effi callback function implementations
%%====================================================================


-spec bind_singleton_boolean(ArgName :: binary(), Value :: binary()) ->
          binary().

bind_singleton_boolean(ArgName, <<"true">>) ->
    <<"$", ArgName/binary, " = 1;\n">>;

bind_singleton_boolean(ArgName, <<"false">>) ->
    <<"$", ArgName/binary, " = 0;\n">>.


-spec bind_singleton_string(ArgName :: binary(), Value :: binary()) ->
          binary().

bind_singleton_string(ArgName, Value) ->
    <<"$", ArgName/binary, " = \"", Value/binary, "\";\n">>.


-spec bind_boolean_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_boolean_list(ArgName, ValueLst) ->

    F =
        fun(<<"true">>) -> "1";
           (<<"false">>) -> "0"
        end,

    S = string:join([ F(Value) || Value <- ValueLst ], ", "),
    B = list_to_binary(S),

    <<"@", ArgName/binary, " = (", B/binary, ");\n">>.


-spec bind_string_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_string_list(ArgName, ValueLst) ->

    Quote =
        fun(X) ->
                "\"" ++ binary_to_list(X) ++ "\""
        end,

    S = string:join([ Quote(Value) || Value <- ValueLst ], ", "),
    B = list_to_binary(S),

    <<"@", ArgName/binary, " = (", B/binary, ");\n">>.


-spec echo_singleton_boolean(ArgName :: binary()) ->
          binary().

echo_singleton_boolean(ArgName) ->
    <<"if( $",
      ArgName/binary,
      " ) {\n",
      "  print \"",
      ?MSG,
      "{\\\"arg_name\\\":\\\"",
      ArgName/binary,
      "\\\",\\\"value\\\":\\\"true\\\"}\\n\";\n",
      "} else {\n"
      "  print \"",
      ?MSG,
      "{\\\"arg_name\\\":\\\"",
      ArgName/binary,
      "\\\",\\\"value\\\":\\\"false\\\"}\\n\";\n",
      "}\n">>.


-spec echo_singleton_string(ArgName :: binary()) ->
          binary().

echo_singleton_string(ArgName) ->
    <<"print \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary, "\\\",\\\"value\\\":\\\"$", ArgName/binary, "\\\"}\\n\";\n">>.


-spec echo_boolean_list(ArgName :: binary()) ->
          binary().

echo_boolean_list(ArgName) ->
    <<"$TMP = join( \", \", map { $_ ? \"\\\"true\\\"\" : \"\\\"false\\\"\" } @", ArgName/binary, " );\n",
      "print \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": [$TMP] }\\n\";\n">>.


-spec echo_string_list(ArgName :: binary()) ->
          binary().

echo_string_list(ArgName) ->
    <<"$TMP = join( \", \", map { \"\\\"$_\\\"\" } @", ArgName/binary, " );\n",
      "print \"", ?MSG, "{ \\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": [$TMP] }\\n\";\n">>.


-spec prefix() ->
          binary().

prefix() ->
    <<>>.


-spec end_of_transmission() ->
          binary().

end_of_transmission() ->
    <<"print \"", ?EOT, "\\n\";\n">>.


-spec suffix() ->
          binary().

suffix() ->
    <<>>.


-spec process_script(Script :: binary()) ->
          binary().

process_script(Script) ->
    Script.


-spec run_extended_script(ExtendedScript :: binary(), Dir :: string(), RunInfo :: _) ->
          {ok, binary(), [#{atom() => _}]} |
          {error, binary()}.

run_extended_script(ExtendedScript, Dir, _) ->

    ScriptFile = string:join([Dir, "__script.pl"], "/"),
    Call = "perl __script.pl",

    ok = file:write_file(ScriptFile, ExtendedScript),

    Port = effi:create_port(Call, Dir),

    effi:listen_port(Port).


-spec get_run_info(Request :: #{atom() => _}) -> [].

get_run_info(_Request) ->
    [].
