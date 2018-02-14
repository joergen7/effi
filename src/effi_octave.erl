%% -*- erlang -*-
%%
%% Erlang foreign function interface.
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
%% @doc The standalone application entry point is {@link main/1}. 
%% The create_port callback defined here is an abstract way to execute child 
%% processes in foreign languages. 
%% There are two foreign language interfaces, both implementing this callback,
%% {@link effi_script} (e.g., Perl, Python) and {@link effi_interact} (e.g.,
%% Bash, R).
%%
%% @end
%% -------------------------------------------------------------------

-module( effi_octave ).

-behaviour( effi ).

%%====================================================================
%% Exports
%%====================================================================

% effi callbacks
-export( [get_extended_script/4, run_extended_script/2] ).

-export( [bind_singleton_string/2,
          echo_singleton_string/1, echo_singleton_boolean/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "effi.hrl" ).

%%====================================================================
%% Effi callback function implementations
%%====================================================================

-spec get_extended_script( ArgTypeLst, RetTypeLst, Script, ArgBindLst ) ->
        binary()
when ArgTypeLst :: [#{ atom() => _ }],
     RetTypeLst :: [#{ atom() => _ }],
     Script     :: binary(),
     ArgBindLst :: [#{ atom() => _ }].

get_extended_script( ArgTypeLst, RetTypeLst, Script, ArgBindLst )
when is_list( ArgTypeLst ),
     is_list( RetTypeLst ),
     is_binary( Script ),
     is_list( ArgBindLst ) ->

  Bind =
    fun( #{ arg_name := ArgName, value := Value }, B ) ->

      TypeInfo = effi:get_type_info( ArgName, ArgTypeLst ),
      #{ arg_type := ArgType, is_list := IsList } = TypeInfo,

      % TODO: cases missing
      case IsList of

        false ->
          case ArgType of
            <<"Str">> ->
              X = bind_singleton_string( ArgName, Value ),
              <<B/binary, X/binary>>
          end

      end
    end,

  Echo =
    fun( TypeInfo, B ) ->

      #{ arg_name := ArgName, arg_type := ArgType, is_list := IsList } = TypeInfo,

      % TODO: cases missing
      case IsList of

        false ->
          case ArgType of

            <<"Str">> ->
              X = echo_singleton_string( ArgName ),
              <<B/binary, X/binary>>;

            <<"Bool">> ->
              X = echo_singleton_boolean( ArgName ),
              <<B/binary, X/binary>>

          end;

        true ->
          case ArgType of
            <<"Str">> ->
              X = echo_string_list( ArgName ),
              <<B/binary, X/binary>>
          end

      end
    end,

  Binding = lists:foldl( Bind, <<>>, ArgBindLst ),
  Echoing = lists:foldl( Echo, <<>>, RetTypeLst ),
  EndOfTransmission = <<"display( '", ?EOT, "' )\n">>,

  <<Binding/binary, "\n",
    Script/binary, "\n",
    Echoing/binary, "\n",
    EndOfTransmission/binary>>.


-spec run_extended_script( ExtendedScript, Dir ) ->
          {ok, binary(), [#{ atom() => _ }]}
        | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string().

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.m"], "/" ),
  Call = "octave __script.m",

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

  <<ArgName/binary, " = '", Value/binary, "';\n">>.


-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"display( ['", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":\"', ", ArgName/binary, ", '\"}\\n'] )\n">>.


echo_singleton_boolean( ArgName )
when is_binary( ArgName ) ->

  <<"if ", ArgName/binary, " display( '", ?MSG, "{\"arg_name\":\"",
    ArgName/binary, "\",\"value\":\"true\"}\\n' ) else display( '", ?MSG,
    "{\"arg_name\":\"", ArgName/binary, "\",\"value\":\"false\"}\\n' ) end\n">>.


-spec echo_string_list( ArgName :: binary() ) -> binary().

echo_string_list( ArgName )
when is_binary( ArgName ) ->

  <<"printf( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":[' )\nfor i = 1:length( ", ArgName/binary,
    " )\n  if i ~= 1 printf( ',' ) end\n  printf( '%s', ",
    ArgName/binary, "{ i } )\nend\nprintf( ']}\\n' )\n">>.

