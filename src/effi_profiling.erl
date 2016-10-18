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
%% @doc Tools for using pegasus kickstart as a lightweight dynamic instrumentation
%% wrapper. Contains logic to generate the wrapper instruction {@link wrapper_call/2}.
%% This is used in the create_port functions in {@link effi_interact} and {@link effi_script}
%% to collect information like peak memory usage, execution time, machine load averages, etc.
%% Also provides a simple configuration data structure (profilingsettings) including
%% accessor functions (e.g., {@link is_on/1}) to provide more flexibility in 
%% changing the data structure.

%% @author Carl Witt <wittcarx@informatik.hu-berlin.de>


-module( effi_profiling ).
-author( "Carl Witt <wittcarx@informatik.hu-berlin.de>" ).
-vsn( "0.1.1-snapshot" ).

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

-export( [get_profiling_settings_from_commandline_args/2, get_profiling_settings/2, is_on/1, wrapper_call/2] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% @doc Gives the prefix needed to wrap a foreign language command with the pegasus-kickstart profiling tool.
-spec wrapper_call( Prof, Dir ) -> string()
when Prof :: profilingsettings(),
     Dir  :: string().

wrapper_call( Prof, Dir )
when is_tuple( Prof ),
     is_list( Dir ) ->
  case is_on( Prof ) of 
      false -> "";
      true ->
        % set the output file of kickstart to be in Dir 
        {profiling, _, ProfileFileName} = Prof,
        OutfileArgument = string:concat( "-l ", filename:join(Dir, ProfileFileName) ),
        % profiler call which to which the actual application is passed
        % connect the profiled process' stdin, stdout, and stderr to the default file descriptors 
        string:concat( "pegasus-kickstart -o - -i - -e - ", OutfileArgument )
  end.

%% get_profiling_settings_from_commandline_args/2
%% @doc Generate a profiling settings data structure, based on the command line arguments passed to effi.
%% Parses the values of the --profiling and --profile-out parameters.
-spec get_profiling_settings_from_commandline_args( OptList, NonOptList ) -> ProfilingSettings
when 
  OptList :: [],
  NonOptList :: [],
  ProfilingSettings :: profilingsettings().

get_profiling_settings_from_commandline_args( OptList, NonOptList ) 
when is_list(OptList),
     is_list(NonOptList) ->
  {profiling, DoProfiling} = lists:keyfind( profiling, 1, OptList ),
  if 
    DoProfiling ->
      {profile_file, OutFileName} = lists:keyfind( profile_file, 1, OptList ),
      {profile_file, _short, _long, {string, DefaultOutName}, _desc} = lists:keyfind( profile_file, 1, effi:get_optspec_lst()),
      ProfileFileName = case OutFileName == DefaultOutName of
        true ->
          [RequestFile, _] = NonOptList,
          string:concat(RequestFile, "_profile.xml");
        false ->
          OutFileName
      end,
      {profiling, true, ProfileFileName};
    true ->
      {profiling, false, ""}
  end.

%% @doc Generates a profiling settings data structure, by explicitly setting 
%% profiling to true or false and specifying the profile output name
-spec get_profiling_settings( DoProfiling, OutFileName ) -> ProfilingSettings
when 
  DoProfiling :: boolean(),
  OutFileName :: string(),
  ProfilingSettings :: profilingsettings().

get_profiling_settings( DoProfiling, OutFileName ) 
when is_atom( DoProfiling ),
     is_list( OutFileName ) ->
  if 
    DoProfiling == true ->
      {profiling, true, OutFileName};
    true ->
      {profiling, false, ""}
  end.

%% is_on/1
%% @doc Extract whether profiling is on or off from the profiling settings data structure
-spec is_on( ProfilingSettings ) -> boolean() 
when
  ProfilingSettings :: profilingsettings().

is_on( {profiling, DoProfiling, _Filename } ) ->
  DoProfiling.

%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

%% @hidden 
from_explicit_test_() ->

  Prof = get_profiling_settings( true, "profile.xml" ),
  {profiling, _, OutFileName} = Prof,
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "profile.xml", OutFileName )
  ].

%% @hidden
from_command_line_default_out_name_test_() ->

  CmdLine = "--profiling request.txt summary.txt",
  {ok, {OptList, NonOptList}} = getopt:parse( effi:get_optspec_lst(), CmdLine ),
  Prof = get_profiling_settings_from_commandline_args(OptList, NonOptList),
  {profiling, _, OutFileName} = Prof,
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "request.txt_profile.xml", OutFileName )
  ].

%% @hidden
from_command_line_test_() ->

  CmdLine = "--profiling --profile-out profile.xml request.txt summary.txt",
  {ok, {OptList, NonOptList}} = getopt:parse( effi:get_optspec_lst(), CmdLine ),
  Prof = get_profiling_settings_from_commandline_args(OptList, NonOptList),
  {profiling, _, OutFileName} = Prof,
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "profile.xml", OutFileName )
  ].

-endif.
