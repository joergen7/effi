-module(effi).

-include( "effi.hrl" ).

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback run( Lang::atom(), Script::string(), Dir::string() ) -> port().


%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [destroy_port/1, run/6] ).




%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% destroy_port/1
%
destroy_port( Port ) when is_port( Port ) ->

  {os_pid, OsPid} = erlang:port_info( Port, os_pid ),

  port_close( Port ),
  
  _Output = os:cmd( io_lib:format( "kill -9 ~p `pgrep -P ~p`", [OsPid, OsPid] ) ),  

  ok.
  

%% run/3
%
run( Lang, Script, Dir, OutList, ParamMap, TypeMap )

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
  apply( FfiType, run, [Lang, [Prefix, Script, Suffix], Dir] ).

