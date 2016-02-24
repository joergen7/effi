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

-module( python ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

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
assignment( ParamName, false, [Value] ) ->
  [ParamName, $=, quote( Value ), $\n];

assignment( ParamName, true, ValueList ) ->
  [ParamName, "=[", string:join( [quote( Value ) || Value <- ValueList], "," ), "]\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["print(\"", ?MSG, "#{\\\"", OutName, "\\\"=>", "[\\\"\"+str(", OutName, ")+\"\\\"]}.\\n\")\n"];

dismissal( OutName, true ) ->
  ["print(\"", ?MSG, "#{\\\"", OutName, "\\\"=>", "[\\\"\"+\"\\\",\\\"\".join(map(str,", OutName, "))+\"\\\"]}.\\n\")\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$', S, $'].
