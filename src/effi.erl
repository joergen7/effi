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
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

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

-callback create_port( Lang::atom(), Script::string(), Dir::string() ) -> port().


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

  % take start time
  Tstart = trunc( os:system_time()/1000000 ),

  % structure info
  OptMap  = get_optmap( OptList ),

  % extract info
  Lang     = maps:get( lang, OptMap ),
  Dir      = maps:get( dir, OptMap ),
  Prefix   = maps:get( prefix, OptMap ),
  OutList  = maps:get( outlist, OptMap ),
  InMap    = maps:get( inmap, OptMap ),
  LMap     = maps:get( lmap, OptMap ),
  FMap     = maps:get( fmap, OptMap ),
  Refactor = maps:get( refactor, OptMap ),

  
  % check pre-conditions
  case check_if_file( InMap, Dir, FMap ) of
    PreMissingList=[_|_] -> {failed, precond, PreMissingList};
    []                   ->
    

      % run
      case run( Lang, Script, Dir, OutList, InMap, LMap ) of
        {failed, ActScript, Out} -> {failed, script_error, ActScript, Out};
        {finished, RMap, Out}    ->
    
          % check post-conditions
          case check_if_file( RMap, Dir, FMap ) of
            PostMissingList=[_|_] -> {failed, postcond, PostMissingList};
            []                    ->
            
              % refactor if desired
              RMap1 = case Refactor of
                        false -> RMap;
                        true  -> refactor_result( RMap, Dir, Prefix, FMap )
                      end,
              
              % take duration
              Tdur = trunc( os:system_time()/1000000 )-Tstart,
          
              % generate summary
              {finished, get_summary( OptList, Script, RMap1, Out, Tstart, Tdur )}
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


check_if_file( PMap, Dir, FMap ) ->
  lists:foldl( fun( P, Acc ) -> acc_check( P, Acc, PMap, Dir, FMap ) end, [],
               maps:keys( PMap ) ).
  
acc_check( P, Acc, PMap, Dir, FMap ) ->

  % check if parameter is of type file
  case maps:get( P, FMap ) of
    false -> Acc;
    true  ->
    
      % get value
      V = maps:get( P, PMap ),
      
      % check if file exists
      case filelib:is_regular( string:join( [Dir, V], "/" ) ) of
        true  -> Acc;
        false -> [V|Acc]
      end
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
acc_info( {refactor, Refactor}, Acc ) -> Acc#{refactor => Refactor};
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
  
  Acc#{inmap => InMap#{Name => Value},
       lmap  => LMap#{Name => false},
       fmap  => FMap#{Name => maps:get( Name, FMap, false )}};
       
acc_info( {listin,   I},        Acc ) ->

  [Name, S1] = string:tokens( I, ":" ),
  ValueList  = string:tokens( S1, "," ),
  InMap      = maps:get( inmap, Acc ),
  LMap       = maps:get( lmap,  Acc ),
  FMap       = maps:get( fmap,  Acc ),

  Acc#{inmap => InMap#{Name => ValueList},
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
               [apply( Lang, assignment, [ParamName, maps:get( ParamName, TypeMap ), maps:get( ParamName, ParamMap )] ),$\n]
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
      listen_port( Port, ActScript, <<LineAcc/binary,PartLine/binary>>, ResultAcc, OutAcc );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line from iolist
      Line = <<LineAcc/binary,PartLine/binary>>,

      case Line of

        % line is a special message
        <<?MSG, AssocStr/binary>> ->
          
          % parse line
          AssocMap = parse_assoc( AssocStr ),

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


%% parse_assoc/1
%
parse_assoc( AssocStr )when is_binary( AssocStr ) ->
  parse_assoc( binary_to_list( AssocStr ) );

parse_assoc( AssocStr )when is_list( AssocStr ) ->

  [Name, S1] = string:tokens( AssocStr, ?COLON ),
  S2 = string:substr( S1, 2, length( S1 )-2 ),
  L1 = string:tokens( S2, ?COMMA ),
  L2 = [string:substr( S, 2, length( S )-2 ) || S <- L1],

  #{Name => L2}.


%% refactor/2
%
refactor_result( RMap, Dir, Prefix, FMap ) ->
  maps:map( fun( P, Value ) -> refactor( P, Value, Dir, Prefix, FMap ) end, RMap ).
  
refactor( P, Value, Dir, Prefix, FMap ) ->

  % check if parameter is of type file
  case maps:get( P, FMap ) of
    false -> Value;
    true  ->

      % create new value
      Value1 = string:join( [Prefix, P, filename:basename( Value )], "_" ),
  
      Orig = filename:absname( string:join( [Dir, Value], "/" ) ),
      Link = string:join( [Dir, Value1], "/" ),
    
      % create symbolic link
      case file:make_symlink( Orig, Link ) of
        {error, R2} -> error( {R2, symlink, [Orig, Link]} );
        ok -> Value1
      end
  end.



-ifdef( TEST ).

greet_bash_test_() ->

  Script   = "out=\"Hello $person\"",
  Dir      = "/tmp",
  OutList  = ["out"],
  ParamMap = #{"person" => "Jorgen"},
  TypeMap  = #{"person" => false, "out" => false},

  {finished, ResultMap, _} = run( bash, Script, Dir, OutList, ParamMap, TypeMap ),
  
  Result = maps:get( "out", ResultMap ),
    
  ?_assertEqual( ["Hello Jorgen"], Result ).

-endif.
