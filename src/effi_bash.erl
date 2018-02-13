%% -*- erlang -*-
%%
%% Effi: Erlang Foreign Function Interface
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
%% @version 0.1.4
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%% @end
%% -------------------------------------------------------------------


-module( effi_bash ).
-behaviour( effi ).

-export( [get_extended_script/4, run_extended_script/2] ).

-include( "effi.hrl" ).

-spec get_extended_script(
            ArgTypeLst     :: [#{ atom() => _ }],
            RetTypeLst     :: [#{ atom() => _ }],
            Script         :: binary(),
            ArgBindLst     :: [#{ atom() => _ }] ) -> binary().

get_extended_script( ArgTypeLst, RetTypeLst, Script, ArgBindLst )
when is_list( ArgTypeLst ),
     is_list( RetTypeLst ),
     is_binary( Script ),
     is_list( ArgBindLst ) ->

  Bind =
    fun( #{ arg_name := ArgName, value := Value }, B ) ->

      % TODO: handle list values
      % TODO: handle boolean values

      X = bind_singleton_string( ArgName, Value ),
      <<B/binary, X/binary>>
    end,

  Echo =
    fun( #{ arg_name := ArgName }, B ) ->

      % TODO: handle list return values
      % TODO: handle boolean return values

      X = echo_singleton_string( ArgName ),
      <<B/binary, X/binary>>
    end,


  Preamble = <<"set -eu -o pipefail\n">>,

  Binding = lists:foldl( Bind, <<>>, ArgBindLst ),

  Echoing = lists:foldl( Echo, <<>>, RetTypeLst ),

  <<Preamble/binary, "\n",
    Binding/binary, "\n",
    Script/binary, "\n",
    Echoing/binary, "\n">>.






-spec run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.sh"], "/" ),
  Call = "bash __script.sh",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).



%%====================================================================
%% Internal functions
%%====================================================================

-spec bind_singleton_string( ArgName, Value ) -> binary()
when ArgName :: binary(),
     Value   :: binary().

bind_singleton_string( ArgName, Value )
when is_binary( ArgName ),
     is_binary( Value ) ->

  <<ArgName/binary, "='", Value/binary, "'\n">>.


-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"echo \"", ?MSG, "{\\\"arg_name\\\":\\\"", ArgName/binary,
    "\\\",\\\"value\\\":\\\"$", ArgName/binary, "\\\"}\"\n">>.