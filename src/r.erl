-module( r ).
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
interpreter() -> "Rscript --vanilla -".


%% prefix/0
%
prefix() -> "".


%% suffix/0
%
suffix() -> "q()".


%% assignment/3
%
assignment( ParamName, false, Value ) ->
  [ParamName, $=, quote( Value ), $\n];
    
assignment( ParamName, true, ValueList ) ->
  [ParamName, "=c(", string:join( lists:map( fun quote/1, ValueList ), "," ), ")\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["cat(\"", ?MSG, OutName, ?COLON, "[\",", OutName, ",\"]\\n\")\n"];
  
dismissal( OutName, true ) ->
  ["cat(\"", ?MSG, OutName, ?COLON, "[\",paste(\"\\\"\",", OutName,
  ",\"\\\"\",collapse=\",\",sep=\"\"),\"]\\n\",sep=\"\")\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$", S, $"].
