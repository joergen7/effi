-module( bash ).
-behaviour( interact ).

-include( "common.hrl" ).

%% ------------------------------------------------------------
%% Callback exports
%% ------------------------------------------------------------

-export( [ffi_type/0, interpreter/0, prefix/0, suffix/0, assignment/3, dismissal/2] ).


%% ------------------------------------------------------------
%% Callback functions
%% ------------------------------------------------------------

%% ffi_type/0
%
ffi_type() -> interact.


%% interpreter/0
%
interpreter() -> "bash".


%% prefix/0
prefix() -> "set -eu -o pipefail".


%% suffix/0
%
suffix() -> "exit".


%% assignment/3
%
assignment( ParamName, false, Value ) ->
  [ParamName, $=, quote( Value ), $\n];
  
assignment( ParamName, true, ValueList ) ->
  [ParamName, "=(", string:join( lists:map( fun quote/1, ValueList ), "," ), ")\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["echo \"", ?MSG, OutName, ?COLON, "[\\\"$", OutName, "\\\"]\"\n"];

dismissal( OutName, true ) ->
  ["TMP=`printf \",\\\"%s\\\"\" ${", OutName,
   "[@]}`\nTMP=${TMP:1}\necho \"", ?MSG, OutName, ?COLON, "[$TMP]\"\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$", S, $"].
