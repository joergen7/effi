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

%% @doc The standalone application entry point is {@link main/1}. 
%% The create_port callback defined here is an abstract way to execute child 
%% processes in foreign languages. 
%% There are two foreign language interfaces, both implementing this callback,
%% {@link effi_script} (e.g., Perl, Python) and {@link effi_interact} (e.g.,
%% Bash, R).

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%% @author Carl Witt <wittcarx@informatik.hu-berlin.de>


-module( effi ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-author( "Carl Witt <wittcarx@informatik.hu-berlin.de>" ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "effi.hrl" ).
-include_lib( "cf_lang/include/cf_protcl.hrl" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [check_run/2, main/1] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% main/1
%% @doc Parses command line arguments and processes the request file using
%% {@link runscript/4}.
-spec main( ArgList::[string()] ) -> ok.

main( [] ) ->
  print_banner(),
  print_usage();

main( CmdLine ) ->
  case getopt:parse( get_optspec_lst(), CmdLine ) of
    {ok, {OptList, [RequestFile, SumFile]}} ->
      case lists:member( version, OptList ) of
        true  -> print_vsn();
        false ->
          case lists:member( help, OptList ) of
            true  ->
              print_banner(),
              print_usage();
            false ->
              case lists:member( cite, OptList ) of
                true  -> print_bibtex();
                false ->
                  {dir, Dir} = lists:keyfind( dir, 1, OptList ),
                  {refactor, Refactor} = lists:keyfind( refactor, 1, OptList ),
                  run_script( Dir, Refactor, RequestFile, SumFile )
              end
          end
      end;
    {ok, {_OptList, NonOptList}} ->
      error( {request_and_summary_expected, NonOptList} );
    {error, {Reason, Data}} ->
      error( {Reason, Data} )
  end.

%% check_run/2
%% @doc Tries to process a parsed request file (Lam, Fa, R, LibMap) using
%% {@link run/2} and handles possible failures. Also measures the execution
%% time. If successful, returns a reply_ok record, otherwise a reply_error
%% record is generated.
-spec check_run( Submit, Dir ) -> #reply_ok{} | #reply_error{}
when Submit :: #submit{},
     Dir    :: string().

check_run( Submit=#submit{ id       = Id,
                           app_line = AppLine,
                           lam_name = LamName,
                           out_vars = OutVars,
                           in_vars  = InVars,
                           arg_map  = ArgMap }, Dir ) ->


  % check pre-conditions
  PreMissingLst = check_if_file( InVars, ArgMap, Dir ),

  case PreMissingLst of
    [_|_] ->
      Msg1 = list_to_binary(
              io_lib:format( "Pre-condition not met: ~s",
                             [string:join( [binary_to_list( M ) || M <- PreMissingLst], ", " )] ) ),
      #reply_error{ id=Id, app_line=AppLine, lam_name=LamName, output=Msg1 };
    [] ->

      % run
      Reply = run( Submit, Dir ),

      case Reply of
        #reply_error{} -> Reply;
        #reply_ok{ result_map=ResultMap } ->

          % check post-conditions
          PostMissingLst = check_if_file( OutVars, ResultMap, Dir ),

          case PostMissingLst of
            [_|_] ->
              Msg2 = list_to_binary(
                       io_lib:format( "Post-condition not met: ~s",
                                      [string:join( [binary_to_list( M ) || M <- PostMissingLst], ", " )] ) ),

              #reply_error{ id=Id, app_line=AppLine, lam_name=LamName,
                            output=Msg2 };
            [] ->
              Reply
          end
      end
  end.

%% opt_spec_list/0
%% @doc Returns the command line parameters that effi can parse, in a format that the getopt module understands. 
get_optspec_lst() ->
  [
   {version,  $v, "version",  undefined,         "Show Effi version"},
   {help,     $h, "help",     undefined,         "Show command line options"},
   {cite,     $c, "cite",     undefined,         "Show Bibtex entry for citation"},
   {dir,      $d, "dir",      {string, "."},     "Working directory"},
   {refactor, $r, "refactor", {boolean, false},  "Refactor output files"}
  ].

%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% print_usage/0
%
print_usage() -> getopt:usage( get_optspec_lst(), "effi", "<requestfile> <summaryfile>" ).

%% print_bibtex_entry/0
%
print_bibtex() -> io:format( "~n~s~n~n", [get_bibtex()] ).

%% get_bibtex/0
%
get_bibtex() ->
  string:join( ["@InProceedings{Brandt2015,",
         "  Title                    = {Cuneiform: A Functional Language for Large Scale Scientific Data Analysis},",
         "  Author                   = {Brandt, J{\"o}rgen and Bux, Marc and Leser, Ulf},",
         "  Booktitle                = {Proceedings of the Workshops of the EDBT/ICDT},",
         "  Year                     = {2015},",
         "  Address                  = {Brussels, Belgium},",
         "  Month                    = {March},",
         "  Pages                    = {17--26},",
         "  Volume                   = {1330},",
         "  Abstract                 = {The need to analyze massive scientific data sets on the one hand and the availability of distributed compute resources with an increasing number of CPU cores on the other hand have promoted the development of a variety of languages and systems for parallel, distributed data analysis. Among them are data-parallel query languages such as Pig Latin or Spark as well as scientific workflow languages such as Swift or Pegasus DAX. While data-parallel query languages focus on the exploitation of data parallelism, scientific workflow languages focus on the integration of external tools and libraries. However, a language that combines easy integration of arbitrary tools, treated as black boxes, with the ability to fully exploit data parallelism does not exist yet. Here, we present Cuneiform, a novel language for large-scale scientific data analysis. We highlight its functionality with respect to a set of desirable features for such languages, introduce its syntax and semantics by example, and show its flexibility and conciseness with use cases, including a complex real-life workflow from the area of genome research. Cuneiform scripts are executed dynamically on the workflow execution platform Hi-WAY which is based on Hadoop YARN. The language Cuneiform, including tool support for programming, workflow visualization, debugging, logging, and provenance-tracing, and the parallel execution engine Hi-WAY are fully implemented.},",
         "  Doi                      = {10.13140/RG.2.1.3547.6561},",
         "  Url                      = {http://ceur-ws.org/Vol-1330/paper-03.pdf}",
         "}"], "\n" ).

print_banner() ->
  io:format( "~s~n~n", [get_banner()] ).

get_banner() ->
  string:join( [
      "   ._,,,,  ,,_,=_",
      "    W   `_@__#__     The Erlang Foreign Function Interface (Effi) allows the",
      "   @P+#   F @F @     execution of functions defined in different programming",
      "  _W   y @  # qF     languages (e.g., Bash, Python, or R) by specifying the",
      "  ^^^^^  P qF  `     function's arguments, body and output values.",
      "",
      "Copyright 2016 Jorgen Brandt <brandjoe@hu-berlin.de>"

    ], "\n" ).

%% print_vsn/0
%
print_vsn() -> io:format( "~s~n", [?VSN] ).


%% run_script/4
%% @doc Parses a request file, processes it using {@link check_run/6} and writes
%% a summary to a file.
-spec run_script( Dir, Refactor, RequestFile, SumFile ) -> ok
when Dir              :: string(),
     Refactor         :: boolean(),
     RequestFile      :: string(),
     SumFile          :: string().

run_script( Dir, Refactor, RequestFile, SumFile ) ->

  % read script from file
  B = case file:read_file( RequestFile ) of
        {error, R1} -> error( {R1, RequestFile} );
        {ok, X}     -> X
      end,

  % parse request
  Submit = cf_protcl:decode( submit, B ),


  % run script
  Reply = check_run( Submit, Dir ),

  % refactor if necessary
  Reply1 = case Reply of
    #reply_ok{} ->
      case Refactor of
        false -> Reply;
        true  ->
          #reply_ok{ id=Id, result_map=ResultMap } = Reply,
          #submit{ id=Id, out_vars=OutVars } = Submit,
          {RefactorLst, [], ResultMap1} = lib_refactor:get_refactoring( OutVars,
            ResultMap, Dir, [Dir], Id ),
          ok = lib_refactor:apply_refactoring( RefactorLst ),
          Reply#reply_ok{ result_map=ResultMap1 }
      end;
    _ -> Reply
  end,

  % write summary
  case file:write_file( SumFile, cf_protcl:encode( Reply1 ) ) of
    {error, R4} -> error( {R4, SumFile} );
    ok          -> ok
  end.


check_if_file( VarLst, ArgMap, Dir ) ->
  lists:foldl( fun( Var, Acc ) -> acc_missing( Var, Acc, ArgMap, Dir ) end, [],
               VarLst ).

acc_missing( #{ is_file := false }, Acc, _ArgMap, _Dir ) ->
  Acc;

acc_missing( #{ name := N, is_file := true }, Acc, ArgMap, Dir ) ->
  #{ N := V } = ArgMap,
  lists:foldl( fun( File, AccIn ) ->
                 acc_file( File, AccIn, Dir )
               end,
               Acc, V ).

acc_file( B, Acc, Dir ) ->
  File = binary_to_list( B ),
  AbsSrc = string:join( [Dir, File], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> [B|Acc];
    true  -> Acc
  end.

%% run/4
%% @doc Creates a port and listens to it, i.e., collects and parses the output
%% from the stdout. Returns a tuple containing a status message (e.g., finished,
%% failed) a result map and the program output.

-spec run( Submit, Dir ) -> Result
when Submit :: #submit{},
     Dir    :: string(),
     Result :: #reply_ok{} | #reply_error{}.

run( Submit=#submit{ out_vars = OutVars,
                     in_vars  = InVars,
                     lang     = Lang,
                     script   = Script,
                     arg_map  = ArgMap }, Dir )
when is_list( Dir ) ->

  % the module to call, depends on the language, e.g., effi_python
  Mod = list_to_atom( "effi_"++atom_to_list( Lang ) ),

  % collect assignments
  Assign = lists:foldl(
             fun( #{ is_list := Pl, name := N }, Acc ) ->
               #{ N := X } = ArgMap,
               C = Mod:assignment( N, Pl, X ),
               <<Acc/binary, C/binary, $\n>>
             end,
             <<"">>, InVars ),

  % collect dismissals
  Dismiss = lists:foldl(
              fun( #{ is_list := Pl, name := N }, Acc ) ->
                C = Mod:dismissal( N, Pl ),
                <<Acc/binary, C/binary, $\n>>
              end,
              <<"">>, OutVars ),

  Script1 = Mod:process( Script ),
  Prefix = Mod:prefix(),
  Suffix = Mod:suffix(),

  ActScript = <<Prefix/binary, $\n, Assign/binary, $\n, Script1/binary, $\n,
                Dismiss/binary, $\n, Suffix/binary, $\n>>,

  % create port
  Port = Mod:create_port( ActScript, Dir ),

  % receive result
  listen_port( Port, Submit, ActScript, <<>>, #{}, <<>> ).


%% listen_port/6
%% @doc Processes the output of the child process (in a foreign language) and
%% builds a result map from it.
-spec listen_port( Port, Submit, ActScript, LineAcc, ResultAcc, OutAcc ) -> Result
when Port      :: port(),
     Submit    :: #submit{},
     ActScript :: binary(),
     LineAcc   :: binary(),
     ResultAcc :: #{binary() => [binary()]},
     OutAcc    :: binary(),
     Result    :: #reply_ok{} | #reply_error{}.

listen_port( Port, Submit=#submit{ id       = Id,
                                   app_line = AppLine,
                                   lam_name = LamName },
             ActScript, LineAcc, ResultAcc, OutAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      LineAcc1 = <<LineAcc/binary, PartLine/binary>>,
      listen_port( Port, Submit, ActScript, LineAcc1, ResultAcc, OutAcc );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line
      Line = <<LineAcc/binary, PartLine/binary>>,

      case Line of

        % line is a special message
        <<?MSG, AssocStr/binary>> ->

          % parse line
          AssocMap = jsone:decode( AssocStr ),

          % continue
          listen_port( Port, Submit, ActScript, <<>>, maps:merge( ResultAcc, AssocMap ), OutAcc );

        % line is an ordinary output
        _ ->

          % continue
          listen_port( Port, Submit, ActScript, <<>>, ResultAcc, <<OutAcc/binary, Line/binary>> )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->

      #reply_ok{ id=Id, result_map=ResultAcc };

    % process failed
    {Port, {exit_status, _}} ->

      #reply_error{ id         = Id,
                    app_line   = AppLine,
                    lam_name   = LamName,
                    act_script = ActScript,
                    output     = OutAcc };

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.

%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

greet_bash_test() ->

  Script  = <<"out=\"Hello $person\"">>,
  OutVars = [#{name    => <<"out">>,
               is_file => false,
               is_list => false}],
  InVars  = [#{name    => <<"person">>,
               is_file => false,
               is_list => false}],
  ArgMap  = #{<<"person">> => [<<"Jörgen">>]},
  Submit  = #submit{ id       = <<"123">>,
                      app_line = 10,
                      lam_name = <<"greet">>,
                      out_vars = OutVars,
                      in_vars  = InVars,
                      lang     = bash,
                      script   = Script,
                      arg_map  = ArgMap },
  Dir     = "/tmp",

  Reply = run( Submit, Dir ),
    
  ?_assertEqual( #reply_ok{ result_map=#{ <<"out">> => [<<"Hello Jörgen">>] } }, Reply ).

-endif.
