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

-export( [checkrun/2, checkrun/8] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------


checkrun( OptList, Script )
when is_list( OptList ),
     is_list( Script ) ->

  % gather info
  {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} = get_info( OptList ),
  
  % check and run
  checkrun( Lang, Dir, Prefix, OutList, InMap, LMap, FMap, Script ).
  
checkrun( Lang, Dir, Prefix, OutList, InMap, LMap, FMap, Script )
when is_atom( Lang ),
     is_list( Dir ),
     is_list( OutList ),
     is_map( InMap ),
     is_map( LMap ),
     is_map( FMap ),
     is_list( Script ) ->
  
  % check pre-conditions
  % TODO
  
  % run
  case run( Lang, Script, Dir, OutList, InMap, LMap ) of
  
    failed        -> failed;
  
    {finished, RMap} ->
    
      % check post-conditions
      % TODO
  
      % rename output files
      case Prefix of
        undef -> {finished, RMap};
        _     -> {finished, refactor_result( RMap, Dir, Prefix, FMap )}
      end
  end.
  

%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% run/6
%
run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) ->

  % create port
  Port = create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap ),
  
  % receive result
  listen_port( Port ).
  
  
%% get_info/1
%
get_info( OptList ) ->
  lists:foldl( fun acc_info/2, {undef, undef, undef, [], #{}, #{}, #{}}, OptList ).


%% acc_info/2
%
acc_info( {lang, Lang1}, {_Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  {Lang1, Dir, Prefix, OutList, InMap, LMap, FMap};
acc_info( {dir, Dir1}, {Lang, _Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  {Lang, Dir1, Prefix, OutList, InMap, LMap, FMap};
acc_info( {prefix, Prefix1}, {Lang, Dir, _Prefix, OutList, InMap, LMap, FMap} ) ->
  {Lang, Dir, Prefix1, OutList, InMap, LMap, FMap};
acc_info( {singout, Name}, {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  Pf = maps:get( Name, FMap, false ),
  {Lang, Dir, Prefix, [Name|OutList], InMap, LMap#{Name => false}, FMap#{Name => Pf}};
acc_info( {listout, Name}, {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  Pf = maps:get( Name, FMap, false ),
  {Lang, Dir, Prefix, [Name|OutList], InMap, LMap#{Name => true}, FMap#{Name => Pf}};
acc_info( {singin, I}, {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  [Name, S1] = string:tokens( I, ":" ),
  Pf = maps:get( Name, FMap, false ),
  {Lang, Dir, Prefix, OutList, InMap#{Name => S1}, LMap#{Name => false}, FMap#{Name => Pf}};
acc_info( {listin, I}, {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  [Name, S1] = string:tokens( I, ":" ),
  L1 = string:tokens( S1, "," ),
  Pf = maps:get( Name, FMap, false ),
  {Lang, Dir, Prefix, OutList, InMap#{Name => L1}, LMap#{Name => true}, FMap#{Name => Pf}};
acc_info( {file, F}, {Lang, Dir, Prefix, OutList, InMap, LMap, FMap} ) ->
  {Lang, Dir, Prefix, OutList, InMap, LMap, FMap#{F => true}}.




  
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

  % run script    
  apply( FfiType, create_port, [Lang, [Prefix, Script, $\n, Suffix], Dir] ).


%% listen_port/1
%
listen_port( Port ) ->
  listen_port( Port, [], #{} ).
  

%% listen_port/3
%
listen_port( Port, LineAcc, ResultAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      listen_port( Port, [PartLine|LineAcc], ResultAcc );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line from iolist
      Line = lists:flatten( lists:reverse( [PartLine|LineAcc] ) ),

      case Line of

        % line is a special message
        ?MSG++AssocStr ->
          
          % parse line
          AssocMap = parse_assoc( AssocStr ),

          % continue
          listen_port( Port, [], maps:merge( ResultAcc, AssocMap ) );

        % line is an ordinary output
        _ ->
        
          % print line
          io:format( "~s~n", [Line] ),

          % continue
          listen_port( Port, [], ResultAcc )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->
      {finished, ResultAcc};

    % process failed
    {Port, {exit_status, _}} ->
      failed;

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.


%% parse_assoc/1
%
parse_assoc( AssocStr ) when is_list( AssocStr ) ->

  [Name, S1] = string:tokens( AssocStr, ?COLON ),
  S2 = string:substr( S1, 2, length( S1 )-2 ),
  L1 = string:tokens( S2, ?COMMA ),
  L2 = [string:substr( S, 2, length( S )-2 ) || S <- L1],

  #{Name => L2}.


%% refactor/2
%
refactor_result( RMap, Dir, Prefix, FMap ) ->
  maps:map( fun( Name, Value ) -> refactor( Name, Value, Dir, Prefix, FMap ) end, RMap ).
  
refactor( Name, Value, Dir, Prefix, FMap ) ->

  % create new value
  Value1 = string:join( [Prefix, filename:basename( Value )], "_" ),
  
  Src = string:join( [Dir, Value], "/" ),
  Dest = string:join( [Dir, Value1], "/" ),
  
  % rename
  case file:rename( Src, Dest ) of
    ok -> Value1;
    {error, Reason} -> error( Reason )
  end.



-ifdef( TEST ).

greet_bash_test_() ->

  Script   = "out=\"Hello $person\"",
  Dir      = "/tmp",
  OutList  = ["out"],
  ParamMap = #{"person" => "Jorgen"},
  TypeMap  = #{"person" => false, "out" => false},

  {finished, ResultMap} = run( bash, Script, Dir, OutList, ParamMap, TypeMap ),
  
  Result = maps:get( "out", ResultMap ),
    
  ?_assertEqual( ["Hello Jorgen"], Result ).

-endif.
