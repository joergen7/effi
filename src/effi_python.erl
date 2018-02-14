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

-module( effi_python ).
-behaviour( effi ).

%%====================================================================
%% Exports
%%====================================================================

% effi callbacks
-export( [get_extended_script/4, run_extended_script/2] ).


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
  EndOfTransmission = <<"print( '", ?EOT, "' )\n">>,


  B1 = binary:replace( Script, <<$\r>>, <<"">>, [global] ),
  B2 = binary:replace( B1, <<$\n>>, <<"\n ">>, [global] ),
  B3 = <<"if True:\n ", B2/binary>>,

  <<Binding/binary, "\n",
    B3/binary, "\n",
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

  ScriptFile = string:join( [Dir, "__script.py"], "/" ),
  Call = "python __script.py",

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

  <<ArgName/binary, " = '", Value/binary, "'\n">>.


-spec echo_singleton_string( ArgName :: binary() ) -> binary().

echo_singleton_string( ArgName )
when is_binary( ArgName ) ->

  <<"print( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":\"'+str( ", ArgName/binary, " )+'\"}\\n' )\n">>.


echo_singleton_boolean( ArgName )
when is_binary( ArgName ) ->

  <<"if ", ArgName/binary, ":\n  print( '", ?MSG, "{\"arg_name\":\"",
    ArgName/binary, "\",\"value\":\"true\"}\\n' )\nelse:\n  print( '", ?MSG,
    "{\"arg_name\":\"", ArgName/binary, "\",\"value\":\"false\"}\\n' )\n">>.


-spec echo_string_list( ArgName :: binary() ) -> binary().

echo_string_list( ArgName )
when is_binary( ArgName ) ->

  <<"print( '", ?MSG, "{\"arg_name\":\"", ArgName/binary,
    "\",\"value\":['+','.join( map( lambda x: '\"%s\"'%(x),", ArgName/binary,
    " ) )+']}\\n')\n">>.