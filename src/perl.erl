%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%%
%% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>


-module( perl ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "0.1.0-snapshot" ).

-behaviour( effi_script ).

-include( "effi.hrl" ).

-export( [ffi_type/0, assignment/3, dismissal/2, shebang/0, extension/0,
          preprocess/1, libpath/1, import/0] ).



libpath( _Path ) -> error( unsupported ).
ffi_type() -> effi_script.
shebang() -> "#!/usr/bin/env perl".
import() -> "".
preprocess( Script ) -> Script.
extension() -> ".pl".

%% assignment/3
%
assignment( ParamName, false, [Value] ) ->
  [ParamName, $=, quote( Value ), $\n];

assignment( ParamName, true, ValueList ) ->
  [ParamName, "=(", string:join( [quote( Value ) || Value <- ValueList], " " ), ")\n"].


%% dismissal/2
%
dismissal( OutName, false ) ->
  ["echo \"", ?MSG, "#{\\\"", OutName, "\\\"=>[{str,\\\"$", OutName, "\\\"}]}.\"\n"];

dismissal( OutName, true ) ->
  ["TMP=`printf \",{str,\\\"%s\\\"}\" ${", OutName,
   "[@]}`\nTMP=${TMP:1}\necho \"", ?MSG, "#{\\\"", OutName, "\\\"=>[$TMP]}.\"\n"].


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% quote/1
%
quote( S ) -> [$", S, $"].
