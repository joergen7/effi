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
%% Effi callback function implementations
%%====================================================================

-spec get_extended_script( ArgTypeLst, RetTypeLst, Script, ArgBindLst ) ->
        binary()
when ArgTypeLst :: [#{ atom() => _ }],
     RetTypeLst :: [#{ atom() => _ }],
     Script     :: binary(),
     ArgBindLst :: [#{ atom() => _ }].

get_extended_script( _ArgTypeLst, _RetTypeLst, _Script, _ArgBindLst ) -> <<>>.


-spec run_extended_script( ExtendedScript, Dir ) ->
          {ok, binary(), [#{ atom() => _ }]}
        | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string().

run_extended_script( _ExtendedScript, _Dir ) -> {error, <<"nyi">>}.