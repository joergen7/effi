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

-module( effi ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback create_port( Script, Dir ) -> port()
when Script :: binary(),
     Dir    :: string().
     

-callback assignment( InVar, IsList, Values ) -> binary()
when InVar  :: binary(),
     IsList :: boolean(),
     Values :: [binary()].

-callback dismissal( OutVar, isList ) -> binary()
when OutVar :: binary(),
     IsList :: boolean().

-callback process( Script ) -> binary()
when Script :: binary().

-callback prefix() -> binary().

-callback suffix() -> binary().

