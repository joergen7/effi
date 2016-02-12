-module( interact ).
-behaviour( effi ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "common.hrl" ).

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback ffi_type() -> atom().
-callback interpreter() -> string().
-callback prefix() -> string().
-callback suffix() -> string().
-callback assignment( ParamName::string(), IsList::boolean(), Value::string() | [string()] ) -> iolist().
-callback dismissal( OutName::string(), IsList::boolean() ) -> iolist().

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

  % get interpreter
  Interpreter = apply( Lang, interpreter, [] ),
  
  % get prefix
  Prefix = apply( Lang, prefix, [] ),
  
  % get suffix
  Suffix = apply( Lang, suffix, [] ),
  
  % complement script
  Script1 = [Prefix, $\n, Script, $\n, Suffix, $\n],

  % run ticket
  Port = open_port( {spawn, Interpreter},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, Dir},
                     {line, ?BUF_SIZE}] ),

  true = port_command( Port, Script1 ),

  Port.
