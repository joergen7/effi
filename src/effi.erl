%% -*- erlang -*-
%%
%% Erlang foreign function interface.
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.4
%% @copyright 2015-2018 Jörgen Brandt
%%
%% @doc The standalone application entry point is {@link main/1}. 
%% The create_port callback defined here is an abstract way to execute child 
%% processes in foreign languages. 
%% There are two foreign language interfaces, both implementing this callback,
%% {@link effi_script} (e.g., Perl, Python) and {@link effi_interact} (e.g.,
%% Bash, R).
%%
%% @end
%% -------------------------------------------------------------------


-module( effi ).


%%====================================================================
%% Exports
%%====================================================================

% handle_request function
-export( [handle_request/2] ).

% helper functions for implementing effi modules
-export( [get_type_info/2, create_port/2, listen_port/1] ).

% escript main functíon
-export( [main/1] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "effi.hrl" ).


%%====================================================================
%% Definitions
%%====================================================================

-define( VSN, "0.1.4" ).
-define( BUF_SIZE, 1024 ).


%%====================================================================
%% Callbacks
%%====================================================================

-callback get_extended_script(
            ArgTypeLst :: [#{ atom() => _ }],
            RetTypeLst :: [#{ atom() => _ }],
            Script     :: binary(),
            ArgBindLst :: [#{ atom() => _ }] ) -> binary().

-callback run_extended_script( ExtendedScript :: binary(), Dir :: string() ) ->
    {ok, binary(), [#{ atom() => _ }]}
  | {error, binary()}.


%%====================================================================
%% Escript main function
%%====================================================================

%% @doc Parses command line arguments and processes the request file.

-spec main( ArgList :: [string()] ) -> ok.

main( CmdLine ) ->

  try

    case getopt:parse( get_optspec_lst(), CmdLine ) of

      {error, Reason} -> error( Reason );

      {ok, {OptLst, []}} ->

        % break if version needs to be displayed
        case lists:member( version, OptLst ) of
          true  -> throw( version );
          false -> ok
        end,

        % break if help needs to be displayed
        case lists:member( help, OptLst ) of
          true  -> throw( help );
          false -> ok
        end,

        % extract working directory
        {dir, Dir} = lists:keyfind( dir, 1, OptLst ),

        % extract request input file
        InputFile =
          case lists:keyfind( input_file, 1, OptLst ) of
            false           -> throw( help );
            {input_file, I} -> I
          end,

        % extract reply output file
        OutputFile =
          case lists:keyfind( output_file, 1, OptLst ) of
            false            -> throw( help );
            {output_file, O} -> O
          end,


        % read script from file
        B =
          case file:read_file( InputFile ) of
            {error, R1}     -> error( {R1, InputFile} );
            {ok, X}         -> X
          end,

        % parse request
        Request = jsone:decode( B, [{keys, atom}] ),

        % handle request
        Reply = handle_request( Request, Dir ),

        % write reply output file
        case file:write_file( OutputFile, jsone:encode( Reply ) ) of
          {error, R2} -> error( {R2, OutputFile} );
          ok          -> ok
        end


    end

  catch
    throw:version -> print_version();
    throw:help    -> print_help()
  end.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Parses a request input file, processes it, and writes the reply output
%%      file.
-spec handle_request( Request, Dir ) -> #{ atom() => _ }
when Request :: #{ atom() => _ },
     Dir :: string().

handle_request( Request, Dir ) ->

  #{ app_id       := AppId,
     lambda       := Lambda,
     arg_bind_lst := ArgBindLst } = Request,

  #{ arg_type_lst := ArgTypeLst,
     ret_type_lst := RetTypeLst,
     script       := Script,
     lang         := Lang } = Lambda,


  % determine language module from lambda
  LangMod = get_lang_mod( Lang ),

  % compute extended script
  ExtendedScript = LangMod:get_extended_script( ArgTypeLst, RetTypeLst, Script,
                                                ArgBindLst ),

  % determine start time
  TStart = os:system_time(),

  % run extended script
  Result =
    case LangMod:run_extended_script( ExtendedScript, Dir ) of

      {ok, _Output, RetBindLst} ->
        #{ status       => <<"ok">>,
           ret_bind_lst => RetBindLst };

      {error, Output} ->
        #{ status          => <<"error">>,
           stage           => <<"run">>,
           extended_script => ExtendedScript,
           output          => Output }

    end,

  % determine duration
  Duration = os:system_time()-TStart,

  % create reply data structure
  #{ app_id          => AppId,
     stat            => #{ t_start  => integer_to_binary( TStart ),
                           duration => integer_to_binary( Duration ) },
     result          => Result }.


-spec get_type_info( ArgName, TypeLst ) -> #{ atom() => _ }
when ArgName :: binary(),
     TypeLst :: [#{ atom() => _ }].

get_type_info( _ArgName, [] ) ->
  error( type_undefined );

get_type_info( ArgName, [H = #{ arg_name := N }|T] )
when is_binary( ArgName ),
     is_binary( N ),
     is_list( T ) ->

  case N of
    ArgName -> H;
    _       -> get_type_info( ArgName, T )
  end.


-spec create_port( Call, Dir ) -> port()
when Call :: string(),
     Dir  :: string().

create_port( Call, Dir )
when is_list( Call ),
     is_list( Dir ) ->

  open_port( {spawn, Call},
             [exit_status,
              stderr_to_stdout,
              binary,
              {cd, Dir},
              {line, ?BUF_SIZE}] ).


-spec listen_port( Port :: port() ) -> 
          {ok, binary(), [#{atom() => binary()}]}
        | {error, binary()}.

listen_port( Port ) ->
  listen_port( Port, <<>>, <<>>, [] ).

%%====================================================================
%% Internal functions
%%====================================================================

%% opt_spec_list/0
%% @doc Returns the command line parameters that effi can parse, in a format that the getopt module understands. 
get_optspec_lst() ->
  [
   {version,     $v, "version",     undefined,     "Show effi version."},
   {help,        $h, "help",        undefined,     "Show command line options."},
   {dir,         $d, "dir",         {string, "."}, "Working directory in which to look for input data and run the request."},
   {input_file,  $i, "input_file",  string,        "Input file holding the effi request (must be specified)."},
   {output_file, $o, "output_file", string,        "Output file into which to write the effi reply (must be specified)."}
  ].


get_banner() ->
  string:join( [
      "   ._,,,,  ,,_,=_",
      "    W   `_@__#__     The Erlang Foreign Function Interface (Effi) allows the",
      "   @P+#   F @F @     execution of functions defined in different programming",
      "  _W   y @  # qF     languages (e.g., Bash, Python, or R) by specifying the",
      "  ^^^^^  P qF  `     function's arguments, body and output values.",
      "",
      "Copyright 2015-2018 Jorgen Brandt <joergen.brandt@onlinehome.de>"

    ], "\n" ).


print_help() ->
  io:format( "~s~n~n", [get_banner()] ),
  getopt:usage( get_optspec_lst(), "effi" ),
  timer:sleep( 10 ),
  io:format( "~nThe input_file and output_file arguments must be specified.~n~n" ).


print_version() ->
  io:format( "application: effi ~s~n", [?VSN] ).


-spec get_lang_mod( B :: binary() ) -> atom().

get_lang_mod( <<"Bash">> )   -> effi_bash;
get_lang_mod( <<"Python">> ) -> effi_python;
get_lang_mod( _ )            -> error( lang_not_recognized ).


-spec listen_port( Port, LineAcc, Output, RetBindLst ) ->
          {ok, binary(), [#{atom() => binary()}]}
        | {error, binary()}
when Port       :: port(),
     LineAcc    :: binary(),
     Output     :: binary(),
     RetBindLst :: [#{atom() => binary()}].

listen_port( Port, LineAcc, Output, RetBindLst )
when is_port( Port ),
     is_binary( LineAcc ),
     is_binary( Output ),
     is_list( RetBindLst ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      LineAcc1 = <<LineAcc/binary, PartLine/binary>>,
      listen_port( Port, LineAcc1, Output, RetBindLst );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line
      Line = <<LineAcc/binary, PartLine/binary>>,

      case Line of

        % line is a special message
        <<?MSG, AssocStr/binary>> ->

          % parse line
          RetBind = jsone:decode( AssocStr, [{keys, atom}] ),

          % continue
          listen_port( Port, <<>>, Output, [RetBind|RetBindLst] );

        % line is an ordinary output
        _ ->

          % continue
          listen_port( Port, <<>>, <<Output/binary, Line/binary>>, RetBindLst )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->
      {ok, Output, RetBindLst};

    % process failed
    {Port, {exit_status, _}} ->
      {error, Output};
      

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.



