-module( effi ).

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

-export( [checkrun/8, get_optmap/1, get_summary/5] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% checkrun/8
%
checkrun( Lang, Dir, Prefix, OutList, InMap, LMap, FMap, Script )
when is_atom( Lang ),
     is_list( Dir ),
     is_list( OutList ),
     is_map( InMap ),
     is_map( LMap ),
     is_map( FMap ),
     is_list( Script ) ->
  
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
  
              % refactor output files if prefix is defined
              case Prefix of
                undef -> {finished, RMap, Out};
                _     -> {finished, refactor_result( RMap, Dir, Prefix, FMap ), Out}
              end
          end
      end
  end.
  
%% get_info/1
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
  
  
%% get_summary/4
%
get_summary( OptList, Ret, Out, Tstart, Tdur ) ->

  OptMap = get_optmap( OptList ),
  
  TaskName = maps:get( taskname, OptMap ),
  Prefix   = maps:get( prefix, OptMap ),
  Lang     = maps:get( lang, OptMap ),
  
    
  #{optlist  => OptList,
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
acc_info( {lang,     Lang},   Acc ) -> Acc#{lang     => Lang};
acc_info( {dir,      Dir},    Acc ) -> Acc#{dir      => Dir};
acc_info( {prefix,   Prefix}, Acc ) -> Acc#{prefix   => Prefix};
acc_info( {taskname, Name},   Acc ) -> Acc#{taskname => Name};
acc_info( {file,     Name},   Acc ) ->

  FMap = maps:get( fmap, Acc ),
  
  Acc#{fmap => FMap#{Name => true}};
  
acc_info( {singout,  Name},   Acc ) ->

  OutList = maps:get( outlist, Acc ),
  LMap    = maps:get( lmap,    Acc ),
  FMap    = maps:get( fmap,    Acc ),

  Acc#{outlist => [Name|OutList],
       lmap    => LMap#{Name => false},
       fmap    => FMap#{Name => maps:get( Name, FMap, false )}};
       
acc_info( {listout,  Name},   Acc ) ->

  OutList = maps:get( outlist, Acc ),
  LMap    = maps:get( lmap,    Acc ),
  FMap    = maps:get( fmap,    Acc ),

  Acc#{outlist => [Name|OutList],
       lmap    => LMap#{Name => true},
       fmap    => FMap#{Name => maps:get( Name, FMap, false )}};

acc_info( {singin,   I},      Acc ) ->

  [Name, Value] = string:tokens( I, ":" ),
  InMap         = maps:get( inmap, Acc ),
  LMap          = maps:get( lmap,  Acc ),
  FMap          = maps:get( fmap,  Acc ),
  
  Acc#{inmap => InMap#{Name => Value},
       lmap  => LMap#{Name => false},
       fmap  => FMap#{Name => maps:get( Name, FMap, false )}};
       
acc_info( {listin,   I},      Acc ) ->

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
