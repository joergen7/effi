-module( python ).
-behaviour( script ).

-include( "common.hrl" ).

%% ------------------------------------------------------------
%% Callback exports
%% ------------------------------------------------------------

-export( [ffi_type/0, assignment/3, dismissal/2, shebang/0, extension/0] ).


%% ------------------------------------------------------------
%% Callback functions
%% ------------------------------------------------------------

%% ffi_type/0
%
ffi_type() -> script.


%% shebang/0
%
shebang() -> "#!/usr/bin/env python".


%% extension/0
%
extension() -> ".py".


%% assignment/3
%
assignment( ParamName, false, Value ) ->
  [ParamName, $=, quote( Value ), $\n];
  
assignment( ParamName, true, ValueList ) ->
  [ParamName, "=[", string:join( [quote( Value ) || Value <- ValueList], "," ), "]\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["print(\"", ?MSG, OutName, ?COLON, "[\\\"\"+", OutName, "+\"\\\"]\\n\")\n"];

dismissal( OutName, true ) ->
  ["print(\"", ?MSG, OutName, ?COLON, "[\"+\"", ?COMMA, "\".join(", OutName, ")+\"]\\n\")\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$', S, $'].
