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

-module( effi_port ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "effi.hrl" ).


%% ------------------------------------------------------------
%% Macros
%% ------------------------------------------------------------

-define( SCRIPT_FILE, "_script" ).
-define( SCRIPT_MODE, 8#700 ).


%% ------------------------------------------------------------
%% Callback function exports
%% ------------------------------------------------------------

-export( [create_script_port/3, create_interact_port/3] ).


%% ------------------------------------------------------------
%% Callback functions
%% ------------------------------------------------------------

%% create_port/3
%
create_script_port( Script, Dir, Interpreter )
when is_binary( Script ),
     is_list( Dir ),
     is_list( Interpreter ) ->

  % compose script file
  ScriptFile = string:join( [Dir, ?SCRIPT_FILE], "/" ),

  % compose script filename
  Call = string:join( [Interpreter, ScriptFile], " " ),

  % create script file
  file:write_file( ScriptFile, Script ),

  % run ticket
  open_port( {spawn, Call},
             [exit_status,
             stderr_to_stdout,
             binary,
             {cd, Dir},
             {line, ?BUF_SIZE}] ).


create_interact_port( Script, Dir, Interpreter )
when is_binary( Script ),
     is_list( Dir ),
     is_list( Interpreter ) ->

  % run ticket
  Port = open_port( {spawn, Interpreter},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, Dir},
                     {line, ?BUF_SIZE}] ),

  % pipe in program
  true = port_command( Port, Script ),

  Port.
