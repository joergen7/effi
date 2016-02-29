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

-module( r ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

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
assignment( ParamName, false, [Value] ) ->
  [ParamName, $=, quote( Value ), $\n];

assignment( ParamName, true, ValueList ) ->
  [ParamName, "=c(", string:join( [quote( Value ) || Value <- ValueList], "," ), ")\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["cat(\"", ?MSG, OutName, ":[\",", OutName, ",\"]\\n\")\n"];

dismissal( OutName, true ) ->
  ["cat(\"", ?MSG, OutName, ":[\",paste(\"\\\"\",", OutName,
  ",\"\\\"\",collapse=\"", ",\",sep=\"\"),\"]\\n\",sep=\"\")\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$", S, $"].
