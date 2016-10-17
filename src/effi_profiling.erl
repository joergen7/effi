%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%%
%% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
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
%% A simple configuration data structure (the type profilingsettings) and
%% accessor functions (e.g., do_profiling) to provide more flexibility in 
%% changing the data structure.

%% @author Carl Witt <wittcarx@informatik.hu-berlin.de>


-module( effi_profiling ).
-author( "Carl Witt <wittcarx@informatik.hu-berlin.de>" ).
-vsn( "0.1.1-snapshot" ).

-define( BUILD, "2016-03-24" ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "effi.hrl" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

% whether to enable profiling and where to write the results to
-type profilingsettings() :: {profiling, Enabled::boolean(), OutFile::string()}.

%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [get_profiling_settings/2, do_profiling/1] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% profiling_settings/2
% Generate a profiling settings data structure, based on whether the profiling is on or off 
-spec get_profiling_settings( DoProfiling, NonOptList ) -> ProfilingSettings
when 
  DoProfiling :: boolean(),
  NonOptList :: [],
  ProfilingSettings :: profilingsettings().

get_profiling_settings( DoProfiling, NonOptList ) ->
  if 
    DoProfiling ->
      [RequestFile, _] = NonOptList,
      ProfileFileName = string:concat(RequestFile, "_profile.xml"),
      io:fwrite(ProfileFileName),
      {profiling, true, ProfileFileName};
    true ->
      {profiling, false, ""}
  end.

%% do_profiling/1
% extract whether profiling is on or off from the profiling settings data structure
-spec do_profiling( ProfilingSettings ) -> boolean() 
when
  ProfilingSettings :: profilingsettings().

do_profiling( {profiling, DoProfiling, _Filename } ) ->
  DoProfiling.

%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

profiling_data_structure_test_() ->

  Prof = get_profiling_settings( true, ["requestfile", "summaryfile"]),
  ?_assertEqual( true, do_profiling(Prof) ).

-endif.
