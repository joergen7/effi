-module( interact ).
-behaviour( effi ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback ffi_type() -> atom().
-callback interpreter() -> string().
-callback prefix() -> string().
-callback suffix() -> string().

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

  % get interpreter
  Interpreter = apply( Lang, interpreter, [] ),
  
  % get prefix
  Prefix = apply( Lang, prefix, [] ),
  
  % get suffix
  Suffix = apply( Lang, suffix, [] ),
  
  % complement script
  Script1 = [Prefix, $\n, Script, $\n, Suffix, $\n],

  % run ticket
  io:format( "Opening ~s port in ~s.~n", [Interpreter, Dir] ),
  Port = open_port( {spawn, Interpreter},
                    [exit_status,
                     stderr_to_stdout,
                     {cd, Dir},
                     {line, 1000000}] ),
  io:format( "Sending data:~n~s~n", [Script1] ),
  true = port_command( Port, Script1 ),

  io:format( "Done creating port.~n" ),

  Port.
