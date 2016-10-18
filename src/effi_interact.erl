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

%% @doc Prototype for interactively interpreted languages, (e.g., Python).
%% As opposed to executing scripts directly ({@see effi_script}).

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>


-module( effi_interact ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "0.1.1-snapshot" ).

-behaviour( effi ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "effi.hrl" ).

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback ffi_type() -> atom().
-callback interpreter() -> string().
-callback prefix() -> string().
-callback suffix() -> string().
-callback assignment( ParamName::string(), IsList::boolean(), Value::string() | [string()] ) -> iodata().
-callback dismissal( OutName::string(), IsList::boolean() ) -> iodata().
-callback preprocess( Script::iodata() ) -> iodata().
-callback libpath( Path::string() ) -> string().


%% ------------------------------------------------------------
%% Callback function exports
%% ------------------------------------------------------------

-export( [create_port/4] ).


%% ------------------------------------------------------------
%% Callback functions
%% ------------------------------------------------------------

%% create_port/4
%
create_port( Mod, Script, Dir, Prof )

when is_atom( Mod ),
     is_list( Script ),
     is_list( Dir ),
     is_tuple( Prof ) ->

  % get interpreter
  Interpreter = apply( Mod, interpreter, [] ),

  % get dynamic instrumentation wrapper
  ProfilingWrapper = effi_profiling:wrapper_call( Prof, Dir ),
    
  % get prefix
  Prefix = apply( Mod, prefix, [] ),

  % get suffix
  Suffix = apply( Mod, suffix, [] ),

  % complement script
  ActScript = string:join( [Prefix, Script, Suffix, ""], "\n" ),

  % run ticket
  Command = case effi_profiling:is_on( Prof ) of 
    true -> string:join( [ProfilingWrapper, Interpreter], " " ); 
    false -> Interpreter 
  end,
  Port = open_port( {spawn, Command},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, Dir},
                     {line, ?BUF_SIZE}] ),

  true = port_command( Port, ActScript ),

  {Port, ActScript}.
