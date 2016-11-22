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

%% @author Jorgen Brandt <brandjoe@hu-berlin.de>


-module( lib_refactor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

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

-spec get_refactoring( VarLst, ArgMap, DestDir, SrcDirLst, Id ) ->
  {RefactorLst, MissingLst, ArgMap1}
when VarLst      :: [#{}],
     ArgMap      :: #{binary() => [binary()]},
     DestDir     :: string(),
     SrcDirLst   :: [string()],
     Id          :: binary(),
     RefactorLst :: [{binary(), binary()}],
     MissingLst  :: [binary()],
     ArgMap1     :: #{binary() => [binary()]}.

get_refactoring( VarLst, ArgMap, DestDir, SrcDirLst, Id ) ->
  lists:foldl( fun( Var, AccIn ) ->
                 acc_refactoring( Var, AccIn, ArgMap, DestDir, SrcDirLst, Id )
               end,
               {[], [], #{}}, VarLst ).

%% apply_refactoring %%

-spec apply_refactoring( Refactor ) -> ok
when Refactor :: {binary(), binary()} | [{binary(), binary()}].

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

-spec acc_refactoring( Var, {RefactorLst1, MissingLst1, ArgMap1}, ArgMap0,
                       DestDir, SrcDirLst, Id ) ->
  {RefactorLst2, MissingLst2, ArgMap2}
when Var          :: #{ is_file := boolean(), is_list := boolean(), name := binary() },
     RefactorLst1 :: [{binary(), binary()}],
     MissingLst1  :: [binary()],
     ArgMap1      :: #{binary() => [binary()]},
     ArgMap0      :: #{binary() => [binary()]},
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     Id           :: binary(),
     RefactorLst2 :: [{binary(), binary()}],
     MissingLst2  :: [binary()],
     ArgMap2      :: #{binary() => [binary()]}.

acc_refactoring( #{ name := N, is_file := false },
                 {RefactorLst1, MissingLst1, ArgMap1},
                 ArgMap0, _DestDir, _SrcDirList, _Id ) ->
  #{ N := V } = ArgMap0,
  {RefactorLst1, MissingLst1, ArgMap1#{ N => V }};

acc_refactoring( #{ name := N, is_file := true },
                 {RefactorLst1, MissingLst1, ArgMap1},
                 ArgMap0, DestDir, SrcDirLst, Id ) ->
  #{ N := FileLst } = ArgMap0,
  {RefactorLst2, MissingLst2, FileLst2} =
    lists:foldl( fun( File, AccIn ) ->
                   acc_file( File, AccIn, DestDir, SrcDirLst, Id )
                 end,
                 {RefactorLst1, MissingLst1, []}, FileLst ),
  {RefactorLst2, MissingLst2, ArgMap1#{ N => lists:reverse( FileLst2 ) }}.

%% acc_file %%

-spec acc_file( File, {RefactorLst, MissingLst, FileLst}, DestDir, SrcDirLst,
                Id ) ->
  {RefactorLst1, MissingLst1, FileLst1}
when File         :: binary(),
     RefactorLst  :: [{binary(), binary()}],
     MissingLst   :: [binary()],
     FileLst      :: [binary()],
     DestDir      :: string(),
     SrcDirLst    :: [string()],
     Id           :: binary(),
     RefactorLst1 :: [{binary(), binary()}],
     MissingLst1  :: [binary()],
     FileLst1     :: [binary()].

acc_file( Filename, {RefactorLst, MissingLst, FileLst}, _DestDir, [], _Id ) ->
  Basename = filename:basename( Filename ),
  {RefactorLst, [Filename|MissingLst], [Basename|FileLst]};

acc_file( Filename, AccIn={RefactorLst, MissingLst, FileLst}, DestDir, [H|T], Id ) ->
  AbsSrc = string:join( [H, binary_to_list( Filename )], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> acc_file( Filename, AccIn, DestDir, T, Id );
    true  ->
      Basename = binary_to_list( filename:basename( Filename ) ),
      DestName = string:join( [binary_to_list( Id ), Basename], "_" ),
      AbsDest = string:join( [DestDir, DestName], "/" ),
      {[{AbsSrc, AbsDest}|RefactorLst], MissingLst, [DestName|FileLst]}
  end.


-ifdef( TEST ).

acc_file_should_add_missing_file_to_missinglst_for_empty_srclst_test() ->
  Filename = <<"some/file/that/does/not/exist">>,
  X = acc_file( Filename, {[], [], []}, "/dest/dir", [], <<"12">> ),
  ?assertEqual( {[], [Filename], [<<"exist">>]}, X ).

acc_file_should_add_missing_file_to_missinglst_for_one_element_srclst_test() ->
  Filename = <<"some/file/that/does/not/exist">>,
  X = acc_file( Filename, {[], [], []}, "/dest/dir", ["/some/src/dir"], <<"12">> ),
  ?assertEqual( {[], [Filename], [<<"exist">>]}, X ).

acc_file_should_add_missing_file_to_missinglst_for_two_element_srclst_test() ->
  Filename = <<"some/file/that/does/not/exist">>,
  X = acc_file( Filename, {[], [], []}, "/dest/dir", ["/some/src/dir", "/some/other/src/dir"], <<"12">> ),
  ?assertEqual( {[], [Filename], [<<"exist">>]}, X ).

acc_refactoring_should_find_missing_files_test() ->
  X = acc_refactoring( #{ name => <<"x">>, is_file => true, is_list => true}, {[], [], #{}}, #{<<"x">> => [<<"1">>, <<"2">>]}, "/dest/dir", [], <<"13">> ),
  ?assertEqual( {[], [<<"2">>, <<"1">>], #{<<"x">> => [<<"1">>, <<"2">>]}}, X ).

-endif.