-module(effi).

-include( "effi.hrl" ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback run( Lang::atom(), Script::string(), Dir::string() ) -> port().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API export
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export( [destroy_port/1, run/3] ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% destroy_port/1
%
destroy_port( Port ) when is_port( Port ) ->

  {os_pid, OsPid} = erlang:port_info( Port, os_pid ),

  port_close( Port ),
  
  _Output = os:cmd( io_lib:format( "kill -9 ~p `pgrep -P ~p`", [OsPid, OsPid] ) ),  

  ok.
  
  
%% run/3
%
run( Lang, Script, Dir )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ) ->

  FfiType = apply( Lang, ffi_type, [] ),
  
  apply( FfiType, run, [Lang, Script, Dir] ).


