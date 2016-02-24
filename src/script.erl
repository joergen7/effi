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

-module( script ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( effi ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "common.hrl" ).


%% ------------------------------------------------------------
%% Macros
%% ------------------------------------------------------------

-define( SCRIPT_FILE, "_script" ).
-define( SCRIPT_MODE, 8#700 ).


%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback ffi_type() -> atom().
-callback assignment( ParamName::string(), IsList::boolean(), Value::string() | [string()] ) -> iolist().
-callback dismissal( OutName::string(), IsList::boolean() ) -> iolist().
-callback shebang() -> string().
-callback extension() -> string().


%% ------------------------------------------------------------
%% Callback function exports
%% ------------------------------------------------------------

-export( [create_port/3] ).


%% ------------------------------------------------------------
%% Callback functions
%% ------------------------------------------------------------

%% create_port/3
%
create_port( Lang, Script, Dir )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ) ->

  % get shebang
  Shebang = apply( Lang, shebang, [] ),

  % complement script with shebang
  ActScript = [Shebang,$\n,Script],

  % get file extension
  Ext = apply( Lang, extension, [] ),

  % compose script filename
  ScriptFile = lists:flatten( [Dir, $/, ?SCRIPT_FILE, Ext] ),

  io:format( "Storing data in ~s:~n~s~n", [ScriptFile, ActScript] ),



  % create script file
  file:write_file( ScriptFile, ActScript ),

  % set file permissions to execute
  file:change_mode( ScriptFile, ?SCRIPT_MODE ),


  % run ticket
  Port = open_port( {spawn, ScriptFile},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, Dir},
                     {line, ?BUF_SIZE}] ),

  io:format( "Done creating port.~n" ),

  Port.
