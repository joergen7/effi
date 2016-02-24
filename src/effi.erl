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
% limitations under the License.

-module( effi ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "common.hrl" ).
-include( "effi.hrl" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback create_port( Lang, Script, Dir ) -> port()
when Lang   :: atom(),
     Script :: string(),
     Dir    :: string().


%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [check_run/2] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------


%% check_run/2
%
check_run( OptList, Script ) ->


  io:format( "Starting check_run ...~n" ),

  % take start time
  Tstart = trunc( os:system_time()/1000000 ),

  % structure info
  OptMap  = get_optmap( OptList ),

  % extract info
  Lang     = maps:get( lang, OptMap ),
  Dir      = maps:get( dir, OptMap ),
  OutList  = maps:get( outlist, OptMap ),
  InMap    = maps:get( inmap, OptMap ),
  LMap     = maps:get( lmap, OptMap ),
  FMap     = maps:get( fmap, OptMap ),

  io:format( "Checking preconditions ...~n" ),

  % check pre-conditions
  PreMissingLst = check_if_file( InMap, Dir, FMap ),

  case PreMissingLst of
    [_|_] ->
      io:format( "Preconditions not met! ~p~naborting ...~n", [PreMissingLst] ),
      {failed, precond, PreMissingLst};
    []    ->

      io:format( "Preconditions met. Firing up ...~n" ),

      % run
      case run( Lang, Script, Dir, OutList, InMap, LMap ) of
        {failed, ActScript, Out} -> {failed, script_error, {ActScript, Out}};
        {finished, RMap, Out}    ->

          % check post-conditions
          PostMissingLst = check_if_file( RMap, Dir, FMap ),

          case PostMissingLst of
            [_|_] -> {failed, postcond, PostMissingLst};
            []    ->

              % take duration
              Tdur = trunc( os:system_time()/1000000 )-Tstart,

              % generate summary
              {finished, get_summary( OptList, Script, RMap, Out, Tstart, Tdur )}
          end
      end
  end.

%% get_optmap/1
%
get_optmap( OptList )when is_list( OptList ) ->

  Acc0 = #{lang     => undef,
           dir      => undef,
           prefix   => undef,
           taskname => undef,
           outlist  => [],
           inmap    => #{},
           lmap     => #{},
           fmap     => #{}},

  lists:foldl( fun acc_info/2, Acc0, OptList ).


%% get_summary/5
%
get_summary( OptList, Script, Ret, Out, Tstart, Tdur )
when is_list( OptList ), is_list( Script ), is_map( Ret ), is_list( Out ),
     is_integer( Tstart ), Tstart >= 0,
     is_integer( Tdur ), Tdur >= 0 ->

  OptMap = get_optmap( OptList ),

  TaskName = maps:get( taskname, OptMap ),
  Prefix   = maps:get( prefix, OptMap ),
  Lang     = maps:get( lang, OptMap ),

  #{optlist  => OptList,
    script   => Script,
    lang     => Lang,
    taskname => TaskName,
    prefix   => Prefix,
    ret      => Ret,
    tstart   => Tstart,
    tdur     => Tdur,
    out      => Out}.


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------


check_if_file( Fa0, Dir, FMap ) ->
  lists:foldl( fun( N, Acc ) -> acc_missing( N, Acc, Fa0, Dir, FMap ) end, [],
               maps:keys( Fa0 ) ).

acc_missing( N, MissingLst, Fa0, Dir, FMap ) ->
  case maps:get( N, FMap ) of
    false -> MissingLst;
    true  ->
      lists:foldl( fun( File, AccIn ) ->
                     acc_file( File, AccIn, Dir )
                   end,
                   MissingLst,
                   maps:get( N, Fa0 ) )
  end.

acc_file( File, MissingLst, Dir ) ->
  AbsSrc = string:join( [Dir, File], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> [File|MissingLst];
    true  -> MissingLst
  end.


%% run/6
%
run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) ->

  % create port
  {Port, ActScript} = create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap ),

  % receive result
  listen_port( Port, ActScript ).



%% acc_info/2
%
acc_info( {lang,     Lang},     Acc ) -> Acc#{lang     => Lang};
acc_info( {dir,      Dir},      Acc ) -> Acc#{dir      => Dir};
acc_info( {prefix,   Prefix},   Acc ) -> Acc#{prefix   => Prefix};
acc_info( {taskname, Name},     Acc ) -> Acc#{taskname => Name};
acc_info( {file,     Name},     Acc ) ->

  FMap = maps:get( fmap, Acc ),

  Acc#{fmap => FMap#{Name => true}};

acc_info( {singout,  Name},     Acc ) ->

  OutList = maps:get( outlist, Acc ),
  LMap    = maps:get( lmap,    Acc ),
  FMap    = maps:get( fmap,    Acc ),

  Acc#{outlist => [Name|OutList],
       lmap    => LMap#{Name => false},
       fmap    => FMap#{Name => maps:get( Name, FMap, false )}};

acc_info( {listout,  Name},     Acc ) ->

  OutList = maps:get( outlist, Acc ),
  LMap    = maps:get( lmap,    Acc ),
  FMap    = maps:get( fmap,    Acc ),

  Acc#{outlist => [Name|OutList],
       lmap    => LMap#{Name => true},
       fmap    => FMap#{Name => maps:get( Name, FMap, false )}};

acc_info( {singin,   I},        Acc ) ->

  [Name, Value] = string:tokens( I, ":" ),
  InMap         = maps:get( inmap, Acc ),
  LMap          = maps:get( lmap,  Acc ),
  FMap          = maps:get( fmap,  Acc ),

  Acc#{inmap => InMap#{Name => [Value]},
       lmap  => LMap#{Name => false},
       fmap  => FMap#{Name => maps:get( Name, FMap, false )}};

acc_info( {listin,   I},        Acc ) ->

  [Name|T] = string:tokens( I, ":" ),
  ValueLst = case T of
               []   -> [];
               [S1] -> string:tokens( S1, "," )
             end,
  InMap      = maps:get( inmap, Acc ),
  LMap       = maps:get( lmap,  Acc ),
  FMap       = maps:get( fmap,  Acc ),

  Acc#{inmap => InMap#{Name => ValueLst},
       lmap  => LMap#{Name => true},
       fmap  => FMap#{Name => maps:get( Name, FMap, false )}}.






%% create_port/6
%
create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ),
     is_list( OutList ),
     is_map( ParamMap ),
     is_map( TypeMap ) ->

  % get Foreign Function Interface type
  FfiType = apply( Lang, ffi_type, [] ),

  % collect assignments
  Prefix = lists:map(
             fun( ParamName ) ->
               Pl       = maps:get( ParamName, TypeMap ),
               ValueLst = maps:get( ParamName, ParamMap ),
               [apply( Lang, assignment, [ParamName, Pl, ValueLst] ),$\n]
             end,
             maps:keys( ParamMap ) ),

  % collect dismissals
  Suffix = lists:map(
             fun( OutName ) ->
               [apply( Lang, dismissal, [OutName, maps:get( OutName, TypeMap )] ), $\n]
             end,
             OutList ),

  ActScript = lists:flatten( [Prefix, Script, $\n, Suffix] ),

  % run script
  Port = apply( FfiType, create_port, [Lang, ActScript, Dir] ),

  {Port, ActScript}.


%% listen_port/2
%
listen_port( Port, ActScript ) ->
  listen_port( Port, ActScript, <<>>, #{}, [] ).


%% listen_port/5
%
listen_port( Port, ActScript, LineAcc, ResultAcc, OutAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      LineAcc1 = <<LineAcc/binary, PartLine/binary>>,
      listen_port( Port, ActScript, LineAcc1, ResultAcc, OutAcc );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line from iolist
      Line = <<LineAcc/binary, PartLine/binary>>,

      case Line of

        % line is a special message
        <<?MSG, AssocStr/binary>> ->

          % parse line
          {ok, Tokens, _} = erl_scan:string( binary_to_list( AssocStr ) ),
          {ok, AssocMap}  = erl_parse:parse_term( Tokens ),

          % continue
          listen_port( Port, ActScript, <<>>, maps:merge( ResultAcc, AssocMap ), OutAcc );

        % line is an ordinary output
        _ ->

          % continue
          listen_port( Port, ActScript, <<>>, ResultAcc, [Line|OutAcc] )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->
      {finished, ResultAcc, lists:reverse( OutAcc )};

    % process failed
    {Port, {exit_status, _}} ->
      {failed, ActScript, lists:reverse( OutAcc )};

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.


%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

greet_bash_test_() ->

  Script   = "out=\"Hello $person\"",
  Dir      = "/tmp",
  OutList  = ["out"],
  ParamMap = #{"person" => ["Jorgen"]},
  TypeMap  = #{"person" => false, "out" => false},

  {finished, ResultMap, _} = run( bash, Script, Dir, OutList, ParamMap, TypeMap ),

  Result = maps:get( "out", ResultMap ),

  ?_assertEqual( ["Hello Jorgen"], Result ).

-endif.
