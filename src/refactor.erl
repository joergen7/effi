%% -*- erlang -*-
%
% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%
% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.-module( refactor ).

-module( refactor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type assoc()       :: #{string() => [string()]}.
-type refactoring() :: {[{string(), string()}], assoc()}.
-type filemap()     :: #{string() => boolean()}.


%% =============================================================================
%% API Functions
%% =============================================================================

-spec get_refactoring( Binding, SrcLst, Dest, Prefix, FMap ) -> Refactoring
when Binding     :: assoc(),
     SrcLst      :: [string()],
     Dest        :: string(),
     Prefix      :: string(),
     FMap        :: filemap(),
     Refactoring :: refactoring() | {error, not_found, string()}.

get_refactoring( Binding, SrcLst, Dest, Prefix, FMap ) ->
  lists:foldl( fun( Param, Acc ) ->
                 acc_refactoring( Param, Acc, maps:get( Param, Binding ),
                                  SrcList, Dest, Prefix, FMap )
               end,
            {[], #{}},
            keys( Binding ) ).


%% =============================================================================
%% Internal Functions
%% =============================================================================

-spec acc_refactoring( Param, Acc0, ValLst, SrcLst, Dest, Prefix, FMap ) -> Acc1
when Param  :: string(),
     Acc0   :: refactoring(),
     ValLst :: [string()],
     SrcLst :: [string()],
     Dest   :: string(),
     Prefix :: string(),
     FMap   :: filemap(),
     Acc1   :: refactoring().

acc_refactoring( _Param, E={error, not_found, _S}, _ValLst, _SrcLst, _Dest, _Prefix, _FMap ) -> E;

acc_refactoring( Param, {LnkLst, Binding}, ValLst, SrcLst, Dest, Prefix, FMap ) ->
  case maps:get( Param, FMap ) of
    false -> {LnkLst, Binding};
    true  ->
      ValLst1 = [filename:basename( Val ) || Val <- ValList],
      AbsDestLst = [string:join( [Dest, Val], "/" ) || Val <- ValLst1],
      AbsSrcLst = lists:foldl( fun( Val, Acc ) -> acc_abslocation( Val, Acc, SrcLst ) end, [], ValList ),


acc_abslocation( Val, Acc, [] ) -> {error, not_found, Val};

acc_abslocation( Val, Acc, [H|T] ) ->
























-export( [refactor_map/5] ).

%% Refactoring of Result Binding Map %%

-spec refactor_map( RMap, DirList, RepoDir, Prefix, FMap ) -> Ret
when RMap    :: #{string() => [string()]},
     DirList :: [string()],
     RepoDir :: string(),
     Prefix  :: string(),
     FMap    :: #{string() => boolean()},
     Ret     :: #{string() => [string()]}.

refactor_map( RMap, DirList, RepoDir, Prefix, FMap ) ->
  maps:map( fun( P, Value ) -> refactor( P, Value, DirList, RepoDir, Prefix, FMap ) end, RMap ).


%% Refactoring Function %%

-spec refactor( P, Value, DirList, DestDir, Prefix, FMap ) -> string()
when P :: string(),
     Value ::

refactor( P, Value, [], DestDir, Prefix, FMap ) ->
  error( {not_found, Value} ).

refactor( P, Value, [H|T], DestDir, Prefix, FMap ) ->

  % check if parameter is of type file
  case maps:get( P, FMap ) of
    false -> Value;
    true  ->

      Orig = filename:absname( string:join( [Dir, Value], "/" ) ),

      case filelib:is_regular( Orig ) of
        false -> refactor( P, Value, T, DestDir, Prefix, FMap );
        true  ->

          % create new value
          Value1 = string:join( [Prefix, P, filename:basename( Value )], "_" ),

          Link = string:join( [RepoDir, Value1], "/" ),

          % create repo directory if necessary
          case filelib:ensure_dir( Link ) of
            {error, R1} -> error( {R1, ensure_dir, Link} );
            ok -> ok
          end,

          % create symbolic link
          case file:make_symlink( Orig, Link ) of
            {error, R2} -> error( {R2, symlink, [Orig, Link]} );
            ok -> Value1
          end
      end
  end.