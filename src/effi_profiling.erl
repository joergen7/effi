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
-type profilingsettings() :: {profiling, 
  Enabled::boolean(),  % whether to instrument processes 
  OutFile::string()    % where to write the profiling results (pegasus-kickstart generates an XML file)
}.

%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [get_profiling_settings_from_commandline_args/2, get_profiling_settings/2, 
          is_on/1, out_file/1, 
          wrapper_call/1] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% @doc Gives the prefix needed to wrap a foreign language command with the pegasus-kickstart profiling tool.
%% If profiling is off, returns an empty string.
-spec wrapper_call( Prof ) -> string()
when Prof :: profilingsettings().

wrapper_call( Prof ) 
when is_tuple( Prof ) ->
  case is_on( Prof ) of 
      false -> "";
      true ->
        % set the output file of kickstart to be in Dir 
        OutfileArgument = string:concat( "-l ", out_file( Prof ) ),
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
      % working directory of effi instantiation
      {dir, Dir} = lists:keyfind( dir, 1, OptList ),
      % the profile file parameter
      {profile_file, OutFileName} = lists:keyfind( profile_file, 1, OptList ),
      % get the default for the profile file parameter
      {profile_file, _short, _long, {string, DefaultOutName}, _desc} = lists:keyfind( profile_file, 1, effi:get_optspec_lst()),
      % if the default name is given then generate a sensible one, otherwise use it as is
      ProfileFileName = case OutFileName == DefaultOutName of
        true ->
          [RequestFile, _] = NonOptList,
          filename:join(Dir, string:concat(RequestFile, "_profile.xml"));
        false ->
          OutFileName
      end,
      get_profiling_settings( true, ProfileFileName );
    true ->
      get_profiling_settings( false, "" )
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
when ProfilingSettings :: profilingsettings().

is_on( {profiling, DoProfiling, _Filename } ) ->
  DoProfiling.

%% out_file/1
%% @doc Extract the name of the profiling results file.
-spec out_file( ProfilingSettings ) -> string()
when ProfilingSettings :: profilingsettings().

out_file( {profiling, _DoProfiling, Filename } ) -> 
  Filename.

%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

%% @hidden 
from_explicit_test_() ->

  Prof = get_profiling_settings( true, "profile.xml" ),
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "profile.xml", out_file(Prof) )
  ].

%% @hidden
from_command_line_default_out_name_test_() ->

  CmdLine = "--profiling --dir workingdir// request.txt summary.txt",
  {ok, {OptList, NonOptList}} = getopt:parse( effi:get_optspec_lst(), CmdLine ),
  Prof = get_profiling_settings_from_commandline_args(OptList, NonOptList),
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "workingdir/request.txt_profile.xml", out_file(Prof) )
  ].

%% @hidden
from_command_line_test_() ->

  CmdLine = "--profiling --profile-out profile.xml request.txt summary.txt",
  {ok, {OptList, NonOptList}} = getopt:parse( effi:get_optspec_lst(), CmdLine ),
  Prof = get_profiling_settings_from_commandline_args(OptList, NonOptList),
  
  [
    ?_assertEqual( true, is_on(Prof) ),
    ?_assertEqual( "profile.xml", out_file(Prof) )
  ].

-endif.
