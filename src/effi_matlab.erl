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

-module( effi_matlab ).

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
  B = effi_octave:get_extended_script( ArgTypeLst,
                                       RetTypeLst,
                                       Script,
                                       ArgBindLst ),
  <<B/binary, "exit\n">>.


-spec run_extended_script( ExtendedScript, Dir ) ->
          {ok, binary(), [#{ atom() => _ }]}
        | {error, binary()}
when ExtendedScript :: binary(),
     Dir            :: string().

run_extended_script( ExtendedScript, Dir )
when is_binary( ExtendedScript ),
     is_list( Dir ) ->

  ScriptFile = string:join( [Dir, "__script.m"], "/" ),
  Call = "matlab -nodisplay -nosplash -r __script.m",

  ok = file:write_file( ScriptFile, ExtendedScript ),

  Port = effi:create_port( Call, Dir ),

  effi:listen_port( Port ).


