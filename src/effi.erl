-module(effi).

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

-export( [run/6, spawn_link_run/6] ).




%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% run/6
%
run( Lang, Script, Dir, OutList, ParamMap, TypeMap )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ),
     is_list( OutList ),
     is_map( ParamMap ),
     is_map( TypeMap ) ->

  % create port
  Port = create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap ),
  
  % receive result
  listen_port( Port, [], #{}, [] ).

%% spawn_link_run/6
%%
%% @doc Starts the specified script in background. Returns the PID of
%%      the process spawned. On script termination, a finished/failed
%%      message is sent to the calling process. Script execution can
%%      be aborted by sending a {'EXIT', Pid, Reason} message to this
%%      process.
%%
spawn_link_run( Lang, Script, Dir, OutList, ParamMap, TypeMap )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ),
     is_list( OutList ),
     is_map( ParamMap ),
     is_map( TypeMap ) ->

  Parent = self(),

  spawn_link(
    fun() ->

      % trap exits
      _OldBoolean = process_flag( trap_exit, true ),

      % run script
      Reply = run( Lang, Script, Dir, OutList, ParamMap, TypeMap ),

      % relay reply
      Parent ! Reply

    end ).

  
    

%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% destroy_port/1
%
destroy_port( Port ) when is_port( Port ) ->

  {os_pid, OsPid} = erlang:port_info( Port, os_pid ),

  port_close( Port ),
  
  _Output = os:cmd( io_lib:format( "kill -9 ~p `pgrep -P ~p`", [OsPid, OsPid] ) ),  

  ok.
  

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



%% listen_port/4
%
listen_port( Port, LineAcc, ResultAcc, OutAcc )

when is_port( Port ),
     is_list( LineAcc ),
     is_map( ResultAcc ),
     is_list( OutAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      listen_port( Port, [PartLine|LineAcc], ResultAcc, OutAcc );

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
          listen_port( Port, [], maps:merge( ResultAcc, AssocMap ), OutAcc );

        % line is an ordinary output
        Line ->

          % continue
          listen_port( Port, [], ResultAcc, [Line|OutAcc] )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->
      {finished, self(), ResultAcc, lists:flatten( lists:reverse( OutAcc ) )};

    % process failed
    {Port, {exit_status, _}} ->
      {failed, self(), lists:flatten( lists:reverse( OutAcc ) )};

    % exit signal received
    {'EXIT', _FromPid, _Reason} ->
      destroy_port( Port );

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





-ifdef( TEST ).

greet_bash_test_() ->

  Script   = "out=\"Hello $person\"",
  Dir      = "/tmp",
  OutList  = ["out"],
  ParamMap = #{"person" => "Jorgen"},
  TypeMap  = #{"person" => false, "out" => false},
  Self      = self(),

  {finished, Self, ResultMap, _} = run( bash, Script, Dir, OutList, ParamMap, TypeMap ),
  
  Result = maps:get( "out", ResultMap ),
    
  ?_assertEqual( ["Hello Jorgen"], Result ).

-endif.
