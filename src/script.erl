-module( script ).
-behaviour( effi ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define( SCRIPT_FILE, "script" ).
-define( SCRIPT_MODE, 8#700 ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback ffi_type() -> atom().
-callback shebang() -> string().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback function exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export( [run/3] ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% run/3
%
run( Lang, Script, Dir )

when is_atom( Lang ),
     is_list( Script ),
     is_list( Dir ) ->

  % get shebang
  Shebang = apply( Lang, shebang, [] ),
  
  % complement script with shebang
  Script1 =  [Shebang,$\n,Script],
  
  % compose script filename
  ScriptFile = lists:flatten( [Dir,$/,?SCRIPT_FILE] ),

  io:format( "Storing data in ~s:~n~s~n", [ScriptFile, Script1] ),

  
  
  % create script file
  file:write_file( ScriptFile, Script1 ),
  
  % set file permissions to execute
  file:change_mode( ScriptFile, ?SCRIPT_MODE ),
  

  % run ticket
  Port = open_port( {spawn, ScriptFile}, [exit_status, stderr_to_stdout, {cd, Dir}, {line, 1000000}] ),

  io:format( "Done creating port.~n" ),

  Port.
