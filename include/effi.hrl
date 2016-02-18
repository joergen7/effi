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

%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

-type result() :: {finished, #{string() => [string()]}, [binary()]}
                | failed.

%% ------------------------------------------------------------
%% Function specifications
%% ------------------------------------------------------------

-spec create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Port
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Port     :: {port(), iolist()}.

-spec run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Result
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Result   :: result().

-spec listen_port( Port, ActScript, LineAcc, ResultAcc, OutAcc ) -> Result
when Port      :: port(),
     ActScript :: iolist(),
     LineAcc   :: binary(),
     ResultAcc :: #{string() => [string()]},
     OutAcc    :: [binary()],
     Result    :: result().
				      
-spec parse_assoc( AssocStr ) -> ResultMap
when AssocStr  :: string() | binary(),
     ResultMap :: #{string => [string()]}.
				       

