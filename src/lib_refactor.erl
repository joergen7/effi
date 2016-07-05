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

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>


-module( lib_refactor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "0.1.0-release" ).

%% =============================================================================
%% API exports
%% =============================================================================

-export( [get_refactoring/5, apply_refactoring/1] ).

%% =============================================================================
%% Includes
%% =============================================================================

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% =============================================================================
%% API Functions
%% =============================================================================

%% get_refactoring %%

-spec get_refactoring( ParamLst, Fa, DestDir, SrcDirLst, R ) ->
  {RefactorLst, MissingLst, Fa1}
when ParamLst    :: [effi:param()],
     Fa          :: #{string() => [effi:str()]},
     DestDir     :: string(),
     SrcDirLst   :: [string()],
     R           :: pos_integer(),
     RefactorLst :: [{string(), string()}],
     MissingLst  :: [string()],
     Fa1         :: #{string() => [effi:str()]}.

get_refactoring( ParamLst, Fa, DestDir, SrcDirLst, R ) ->
  lists:foldl( fun( Param, AccIn ) ->
                 acc_refactoring( Param, AccIn, Fa, DestDir, SrcDirLst, R )
               end,
               {[], [], #{}}, ParamLst ).

%% apply_refactoring %%

-spec apply_refactoring( Refactor ) -> ok
when Refactor :: {string(), string()} | [{string(), string()}].

apply_refactoring( RefactorLst ) when is_list( RefactorLst ) ->
  lists:foreach( fun apply_refactoring/1, RefactorLst );

apply_refactoring( {Existing, New} ) ->

  case filelib:ensure_dir( New ) of
    {error, R1} -> error( {R1, ensure_dir, New} );
    ok          -> ok
  end,

  case file:make_symlink( filename:absname( Existing ), New ) of
    {error, eexist} -> ok;
    {error, R2}     -> error( {R2, make_symlink, [Existing, New]} );
    ok              -> ok
  end.




%% =============================================================================
%% Internal Functions
%% =============================================================================

%% acc_refactoring %%

-spec acc_refactoring( Param, {RefactorLst, MissingLst, Fa1}, Fa0, DestDir,
                       SrcDirLst, R ) ->
  {RefactorLst1, MissingLst1, Fa2}
when Param        :: effi:param(),
     RefactorLst  :: [{string(), string()}],
     MissingLst   :: [string()],
     Fa1          :: #{string() => [effi:str()]},
     Fa0          :: #{string() => [effi:str()]},
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     R            :: pos_integer(),
     RefactorLst1 :: [{string(), string()}],
     MissingLst1  :: [string()],
     Fa2          :: #{string() => [effi:str()]}.

acc_refactoring( {param, {name, N, false}, _Pl}, {RefactorLst, MissingLst, Fa1},
                 Fa0, _DestDir, _SrcDirList, _R ) ->
  {RefactorLst, MissingLst, Fa1#{N => maps:get( N, Fa0 )}};

acc_refactoring( {param, {name, N, true}, _Pl}, {RefactorLst, MissingLst, Fa1},
                 Fa0, DestDir, SrcDirLst, R ) ->
  FileLst = maps:get( N, Fa0 ),
  {RefactorLst1, MissingLst1, FileLst1} =
    lists:foldl( fun( File, AccIn ) ->
                   acc_file( File, AccIn, DestDir, SrcDirLst, R )
                 end,
                 {RefactorLst, MissingLst, []}, FileLst ),
  FileLst2 = lists:reverse( FileLst1 ),
  {RefactorLst1, MissingLst1, Fa1#{N => FileLst2}}.

%% acc_file %%

-spec acc_file( File, {RefactorLst, MissingLst, FileLst}, DestDir, SrcDirLst,
                R ) ->
  {RefactorLst1, MissingLst1, FileLst1}
when File         :: string() | effi:str(),
     RefactorLst  :: [{string(), string()}],
     MissingLst   :: [string()],
     FileLst      :: [effi:str()],
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     R            :: pos_integer(),
     RefactorLst1 :: [{string(), string()}],
     MissingLst1  :: [string()],
     FileLst1     :: [effi:str()].

acc_file( {str, Filename}, Acc, DestDir, SrcDirLst, R ) ->
  Acc1 = acc_file( Filename, Acc, DestDir, SrcDirLst, R ),
  {RefactorLst, MissingLst, [H|T]} = Acc1,
  {RefactorLst, MissingLst, [{str, H}|T]};

acc_file( Filename, {RefactorLst, MissingLst, FileLst}, _DestDir, [], _R ) ->
  Basename = filename:basename( Filename ),
  {RefactorLst, [Filename|MissingLst], [Basename|FileLst]};

acc_file( Filename, AccIn={RefactorLst, MissingLst, FileLst}, DestDir, [H|T], R ) ->
  AbsSrc = string:join( [H, Filename], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> acc_file( Filename, AccIn, DestDir, T, R );
    true  ->
      Basename = filename:basename( Filename ),
      DestName = string:join( [integer_to_list( R ), Basename], "_" ),
      AbsDest = string:join( [DestDir, DestName], "/" ),
      {[{AbsSrc, AbsDest}|RefactorLst], MissingLst, [DestName|FileLst]}
  end.


-ifdef( TEST ).

acc_file_should_add_missing_file_to_missinglst_for_empty_srclst_test() ->
  Filename = "some/file/that/does/not/exist",
  X = acc_file( Filename, {[], [], []}, "/dest/dir", [], 12 ),
  ?assertEqual( {[], [Filename], ["exist"]}, X ).

acc_file_should_add_missing_file_to_missinglst_for_one_element_srclst_test() ->
  Filename = "some/file/that/does/not/exist",
  X = acc_file( Filename, {[], [], []}, "/dest/dir", ["/some/src/dir"], 12 ),
  ?assertEqual( {[], [Filename], ["exist"]}, X ).

acc_file_should_add_missing_file_to_missinglst_for_two_element_srclst_test() ->
  Filename = "some/file/that/does/not/exist",
  X = acc_file( Filename, {[], [], []}, "/dest/dir", ["/some/src/dir", "/some/other/src/dir"], 12 ),
  ?assertEqual( {[], [Filename], ["exist"]}, X ).

acc_file_should_add_missing_file_to_missinglst_for_str_test() ->
  Filename = "some/file/that/does/not/exist",
  X = acc_file( {str, Filename}, {[], [], []}, "/dest/dir", [], 12 ),
  ?assertEqual( {[], [Filename], [{str, "exist"}]}, X ).

acc_refactoring_should_work_test() ->
  X = acc_refactoring( {param, {name, "x", true}, true}, {[], [], #{}}, #{"x" => [{str, "1"}, {str, "2"}]}, "/dest/dir", [], 13 ),
  ?assertEqual( {[], ["2", "1"], #{"x" => [{str, "1"}, {str, "2"}]}}, X ).

-endif.