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
          wrapper_call/1, effi_arguments_for/1 ] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% @doc Gives the prefix needed to wrap a foreign language command with the pegasus-kickstart profiling tool.
%% If profiling is off, returns an empty string.
%% e.g. {profiling, true, "./path/to/out.xml"} => "pegasus-kickstart -o - -l ./path/to/out.xml"
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

%% effi_arguments_for/1
%% @doc generates the command line arguments for a given profiling setting, 
%% e.g. {profiling, true, "./example.xml"} => "--profiling --profile-out ./example.xml" 
%% This does the opposite of {@link get_profiling_settings_from_commandline_args/2}.
-spec effi_arguments_for( ProfilingSettings ) -> string()
when ProfilingSettings :: profilingsettings().

effi_arguments_for( {profiling, DoProfiling, Filename} ) ->
  case DoProfiling of
    false -> "";
    true ->
      % Get the effi command line switch for profiling
      {profiling, _, ProfSwitchName, _, _} = lists:keyfind( profiling, 1, effi:get_optspec_lst() ),
      ProfArg = string:concat("--", ProfSwitchName),

      ProfOutArg = case out_file_name_fallback( Filename, none ) of
        % the name is the default, not specifying it will have the same effect
        {refactored, _} -> "";
        % the name is sensible (not the default)
        {unchanged, _} -> 
          % Get the effi command line parameter for profile output file
          {profile_file, _, ProfOutName, _, _} = lists:keyfind( profile_file, 1, effi:get_optspec_lst() ),
          string:join(["--", ProfOutName, " ", Filename], "")
      end,

      string:join( [ProfArg, ProfOutArg], " " )
  end.
  
%% get_profiling_settings_from_commandline_args/2
%% @doc Generate a profiling settings data structure, based on the command line arguments passed to effi.
%% Parses the values of the --profiling and --profile-out parameters.
%% This does the opposite of {@link effi_arguments_for/1}.
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
      [RequestFile, _] = NonOptList,
      {_changed, ProfileFileName} = out_file_name_fallback( OutFileName, filename:join( Dir, filename:basename( RequestFile ) ) ),
      get_profiling_settings( true, ProfileFileName );
    true ->
      get_profiling_settings( false, "" )
  end.

%% out_file_name_fallback/2
%% @doc checks whether the given file name equals the command line arguments default file name
%% and generates one based on a given base file name: e.g., the name of the request file, by 
%% appending the suffix _profile.xml. If the given name is not the default, leaves it unchanged.
%% e.g., ("&lt;requestfile&gt;_profile.xml", "./path/to/request_file") => {refactored, "./path/to/request_file_profile.xml"}
%% e.g., ("./path/to/profile.xml", "./path/to/request_file") => {unchanged, "./path/to/profile.xml"}
-spec out_file_name_fallback( OutFileName, BaseFileName ) -> {atom(), string()}
when 
  OutFileName :: string(),
  BaseFileName :: string().

out_file_name_fallback( OutFileName, BaseFileName ) ->
  % get the default for the profile file parameter
  {profile_file, _short, _long, {string, DefaultOutName}, _desc} = lists:keyfind( profile_file, 1, effi:get_optspec_lst()),
  % if the default name is given then generate one, otherwise use it as is
  case OutFileName == DefaultOutName of
    true ->
      {refactored, string:concat(BaseFileName, "_profile.xml")};
    false ->
      {unchanged, OutFileName}
  end.

%% get_profiling_settings/2
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
  Prof = get_profiling_settings_from_commandline_args( OptList, NonOptList ),
  
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

%% @hidden
effi_arguments_for_test_() ->
  ?_assertEqual( "--profiling --profile-out ./example.xml", effi_arguments_for({profiling, true, "./example.xml"}) ).

%% @hidden
out_file_name_fallback_test_() ->
  [
    ?_assertEqual({refactored, "./path/to/request_file_profile.xml"}, out_file_name_fallback("<requestfile>_profile.xml", "./path/to/request_file")), 
    ?_assertEqual({unchanged, "./path/to/profile.xml"}, out_file_name_fallback("./path/to/profile.xml", "./path/to/request_file"))
  ].


-endif.
