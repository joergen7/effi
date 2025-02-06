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

-module(effi_r).
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


-spec bind_singleton_boolean(ArgName :: binary(), Value :: <<_:32, _:_*8>>) -> <<_:64, _:_*8>>.

bind_singleton_boolean(ArgName, <<"true">>) ->
    <<ArgName/binary, " = TRUE\n">>;

bind_singleton_boolean(ArgName, <<"false">>) ->
    <<ArgName/binary, " = FALSE\n">>.


-spec bind_singleton_string(ArgName :: binary(), Value :: binary()) -> <<_:48, _:_*8>>.

bind_singleton_string(ArgName, Value) ->
    <<ArgName/binary, " = \"", Value/binary, "\"\n">>.


-spec bind_boolean_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_boolean_list(ArgName, ValueLst) ->

    F =
        fun(<<"true">>) -> "TRUE";
           (<<"false">>) -> "FALSE"
        end,

    S = string:join([ F(Value) || Value <- ValueLst ], ", "),
    B = list_to_binary(S),

    <<ArgName/binary, " = c( ", B/binary, " )\n">>.


-spec bind_string_list(ArgName :: binary(), Value :: [binary()]) ->
          binary().

bind_string_list(ArgName, ValueLst) ->

    Quote =
        fun(X) ->
                "\"" ++ binary_to_list(X) ++ "\""
        end,

    S = string:join([ Quote(Value) || Value <- ValueLst ], ", "),
    B = list_to_binary(S),

    <<ArgName/binary, " = c( ", B/binary, " )\n">>.


-spec echo_singleton_boolean(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_singleton_boolean(ArgName) ->
    <<"if( ", ArgName/binary, " ) {\n",
      "  cat( \"", ?MSG, "{\\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": \\\"true\\\"}\\n\", sep=\"\" )\n",
      "} else {\n",
      "  cat( \"", ?MSG, "{\\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": \\\"false\\\"}\\n\", sep=\"\" )\n",
      "}\n">>.


-spec echo_singleton_string(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_singleton_string(ArgName) ->
    <<"cat( \"", ?MSG, "{\\\"arg_name\\\": \\\"", ArgName/binary, "\\\", \\\"value\\\": \\\"\", ", ArgName/binary, ", \"\\\"}\\n\", sep=\"\" )\n">>.


-spec echo_boolean_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_boolean_list(ArgName) ->
    <<"cat( \"", ?MSG, "{\\\"arg_name\\\": \\\"", ArgName/binary,
      "\\\", \\\"value\\\": [\", Reduce( function( x, y ) paste( x, y, sep=\",\" ), Map( function( x ) if( x ) \"\\\"true\\\"\" else \"\\\"false\\\"\", ",
      ArgName/binary, " ) ), \"] }\\n\", sep=\"\" )\n">>.


-spec echo_string_list(ArgName :: binary()) -> <<_:64, _:_*8>>.

echo_string_list(ArgName) ->
    <<"cat( \"", ?MSG, "{\\\"arg_name\\\": \\\"", ArgName/binary,
      "\\\", \\\"value\\\": [\", Reduce( function( x, y ) paste( x, y, sep=\",\" ), Map( function( x ) paste( \"\\\"\", x, \"\\\"\", sep=\"\" ), ",
      ArgName/binary, " ) ), \"] }\\n\", sep=\"\" )\n">>.


-spec prefix() -> <<>>.

prefix() ->
    <<>>.


-spec end_of_transmission() -> <<_:136>>.

end_of_transmission() ->
    <<"cat( \"", ?EOT, "\\n\" )\n">>.


-spec suffix() -> <<>>.

suffix() ->
    <<>>.


-spec process_script(Script :: binary()) ->
          binary().

process_script(Script) ->
    Script.


-spec run_extended_script(ExtendedScript :: binary(), Dir :: string(), RunInfo :: _) ->
          {ok, binary(), [#{atom() => binary()}]} |
          {error, binary()}.

run_extended_script(ExtendedScript, Dir, _) ->

    ScriptFile = string:join([Dir, "__script.R"], "/"),
    Call = "Rscript --vanilla __script.R",

    ok = file:write_file(ScriptFile, ExtendedScript),

    Port = effi:create_port(Call, Dir),

    effi:listen_port(Port).


-spec get_run_info(Request :: #{atom() => _}) -> [].

get_run_info(_Request) ->
    [].
