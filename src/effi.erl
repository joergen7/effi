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

-module( effi ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "0.1.0" ).

-define( BUILD, "2016-03-24" ).

%% ------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------

-include( "effi.hrl" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% ------------------------------------------------------------
%% Callback definitions
%% ------------------------------------------------------------

-callback create_port( Lang, Script, Dir ) -> {port(), string()}
when Lang   :: atom(),
     Script :: string(),
     Dir    :: string().

%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

-type result()  :: {finished, #{atom() => term()}}
                 | {failed, atom(), pos_integer(), term()}.

-type lam()     :: {lam, LamLine::pos_integer(), Name::string(),
                         S::sign(), B::forbody()}.
-type sign()    :: {sign, Lo::[param()], Li::[param()]}.
-type param()   :: {param, M::name(), Pl::boolean()}.
-type name()    :: {name, N::string(), Pf::boolean()}.
-type forbody() :: {forbody, L::lang(), S::string()}.
-type lang()    :: bash | python | r.
-type str()     :: {str, S::string()}.

%% ------------------------------------------------------------
%% API export
%% ------------------------------------------------------------

-export( [check_run/5, main/1] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------

%% main/1
%
-spec main( ArgList::[string()] ) -> ok.

main( [] ) ->
  print_banner(),
  print_usage();

main( CmdLine ) ->
  case getopt:parse( get_optspec_lst(), CmdLine ) of
    {ok, {OptList, NonOptList}} ->
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
                  runscript( Dir, Refactor, NonOptList )
              end
          end
      end;
    {error, {Reason, Data}} ->
      error( {Reason, Data} )
  end.

%% check_run/5
%
-spec check_run( Lam, Fa, R, Dir, LibMap ) -> result()
when Lam    :: lam(),
     Fa     :: #{string() => [str()]},
     R      :: pos_integer(),
     Dir    :: string(),
     LibMap :: #{atom() => [string()]}.

check_run( Lam, Fa, R, Dir, LibMap )
when is_tuple( Lam ),
     is_map( Fa ),
     is_integer( R ), R > 0,
     is_list( Dir ),
     is_map( LibMap ) ->

  % take start time
  Tstart = trunc( os:system_time()/1000000 ),

  {lam, _Line, _LamName, Sign, _Body} = Lam,
  {sign, Lo, Li} = Sign,

  % check pre-conditions
  PreMissingLst = check_if_file( Li, Fa, Dir ),

  case PreMissingLst of
    [_|_] -> {failed, precond, R, PreMissingLst};
    []    ->

      % run
      case run( Lam, Fa, Dir, LibMap ) of
        {failed, script_error, Data} -> {failed, script_error, R, Data};
        {finished, RMap, Out}        ->

          % check post-conditions
          PostMissingLst = check_if_file( Lo, RMap, Dir ),

          case PostMissingLst of
            [_|_] -> {failed, postcond, R, PostMissingLst};
            []    ->

              % take duration
              Tdur = trunc( os:system_time()/1000000 )-Tstart,

              % generate summary
              {finished, get_summary( Lam, Fa, R, RMap, Out, Tstart, Tdur )}
          end
      end
  end.


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

%% print_usage/0
%
print_usage() -> getopt:usage( get_optspec_lst(), "effi", "<requestfile> <summaryfile>" ).

%% opt_spec_list/0
%
get_optspec_lst() ->
  [
   {version,  $v, "version",  undefined,         "Show Effi version"},
   {help,     $h, "help",     undefined,         "Show command line options"},
   {cite,     $c, "cite",     undefined,         "Show Bibtex entry for citation"},
   {dir,      $d, "dir",      {string, "."},     "Working directory"},
   {refactor, $r, "refactor", {boolean, false},  "Refactor output files"}
  ].

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
print_vsn() -> io:format( "~s~n", [get_vsn()] ).

get_vsn() ->
  {vsn, Vsn} = lists:keyfind( vsn, 1, module_info( attributes ) ),
  Vsn.

%% runscript/3
%
runscript( Dir, Refactor, [RequestFile, SumFile] ) ->

  % read script from file
  B = case file:read_file( RequestFile ) of
        {error, R1} -> error( {R1, RequestFile} );
        {ok, X}     -> X
      end,

  % parse line
  {ok, Tokens, _} = erl_scan:string( binary_to_list( B ) ),
  {Lam, Fa, R} = case erl_parse:parse_term( Tokens ) of
                   {error, Reason} -> error( Reason );
                   {ok, Y}         -> Y
                 end, 

  % run script
  Summary = case effi:check_run( Lam, Fa, R, Dir ) of

              {failed, script_error, R, {ActScript, Out}}     ->

                % print tool output
                io:format( "[out]~n" ),
                lists:foreach( fun( Line ) ->
                                 io:format( "~s~n", [Line] )
                               end,
                               Out ),

                % print actual script
                io:format( "[script]~n" ),
                _ = lists:foldl( fun( Line, N ) ->
                                   ok = io:format( "~4.B  ~s~n", [N, Line] ),
                               N+1
                             end,
                             1, re:split( ActScript, "\n" ) ),

                % throw error
                error( script_error );

              {failed, R2, R, MissingList} ->
                error( {R2, MissingList} );

              {finished, Sum} -> 
                case Refactor of
                  false -> Sum;
                  true  ->
                    RMap = maps:get( ret, Sum ),
                    {lam, _Line, _LamName, Sign, _Body} = Lam,
                    {sign, Lo, _Li} = Sign,
                    {RefactorLst, [], RMap1} = refactor:get_refactoring( Lo, RMap, Dir, [Dir], R ),
                    ok = refactor:apply_refactoring( RefactorLst ),
                    maps:put( ret, RMap1, Sum )
                end

            end,


  % write summary
  case file:write_file( SumFile, io_lib:format( "~p.~n", [Summary] ) ) of
    {error, R4} -> error( {R4, SumFile} );
    ok          -> ok
  end;



runscript( _Dir, _Refactor, NonOptList ) ->
  error( {request_and_summary_expected, NonOptList} ).

%% get_summary/5
%
-spec get_summary( Lam, Fa, R, Ret, Out, Tstart, Tdur ) -> #{atom() => term()}
when Lam    :: lam(),
     Fa     :: #{string => [str()]},
     R      :: pos_integer(),
     Ret    :: #{string() => [str()]},
     Out    :: [binary()],
     Tstart :: integer(),
     Tdur   :: integer().

get_summary( Lam, Fa, R, Ret, Out, Tstart, Tdur )
when is_tuple( Lam ), is_map( Fa ), is_map( Ret ), is_list( Out ),
     is_integer( Tstart ), Tstart >= 0, is_integer( Tdur ), Tdur >= 0,
     is_integer( R ), R > 0 ->

  #{lam      => Lam,
    arg      => Fa,
    id       => R,
    ret      => Ret,
    tstart   => Tstart,
    tdur     => Tdur,
    out      => Out}.




check_if_file( ParamLst, Fa, Dir ) ->
  lists:foldl( fun( Param, Acc ) -> acc_missing( Param, Acc, Fa, Dir ) end, [],
               ParamLst ).

acc_missing( {param, {name, _N, false}, _IsLst}, Acc, _Fa, _Dir ) ->
  Acc;

acc_missing( {param, {name, N, true}, _IsLst}, Acc, Fa, Dir ) ->
      lists:foldl( fun( File, AccIn ) ->
                     acc_file( File, AccIn, Dir )
                   end,
                   Acc,
                   maps:get( N, Fa ) ).

acc_file( {str, File}, Acc, Dir ) ->
  AbsSrc = string:join( [Dir, File], "/" ),
  case filelib:is_regular( AbsSrc ) of
    false -> [File|Acc];
    true  -> Acc
  end.

-spec run( Lam, Fa, Dir, LibMap ) -> Result
when Lam    :: lam(),
     Fa     :: #{string() => [str()]},
     Dir    :: string(),
     LibMap :: #{atom() => [string()]},
     Result :: {finished, #{string() => [str()]}, [binary()]}
             | {failed, script_error, {iolist(), [binary()]}}.


%% run/4
%
run( Lam, Fa, Dir, LibMap )
when is_tuple( Lam ),
     is_map( Fa ),
     is_list( Dir ),
     is_map( LibMap ) ->

  % create port
  {Port, ActScript} = create_port( Lam, Fa, Dir, LibMap ),

  % receive result
  listen_port( Port, ActScript ).


%% create_port/3
%
-spec create_port( Lam, Fa, Dir, LibMap ) -> {port(), string()}
when Lam    :: lam(),
     Fa     :: #{string() => [str()]},
     Dir    :: string(),
     LibMap :: #{atom() => [string()]}.

create_port( Lam, Fa, Dir, LibMap )
when is_tuple( Lam ),
     is_map( Fa ),
     is_list( Dir ),
     is_map( LibMap ) ->

  {lam, _Line, _LamName, Sign, Body} = Lam,
  {forbody, Lang, Script} = Body,
  {sign, Lo, Li} = Sign,

  % get Foreign Function Interface type
  FfiType = apply( Lang, ffi_type, [] ),

  % include lib paths
  LibPath = [[apply( Lang, libpath, [P] ), $\n] || P <- maps:get( Lang, LibMap )],

  % collect assignments
  Assign = lists:map(
             fun( {param, {name, N, _Pf}, Pl} ) ->
               X = maps:get( N, Fa ),
               X1 = [S ||{str, S} <- X],
               [apply( Lang, assignment, [N, Pl, X1] ), $\n]
             end,
             Li ),

  % collect dismissals
  Suffix = lists:map(
             fun( {param, {name, N, _Pf}, Pl} ) ->
               [apply( Lang, dismissal, [N, Pl] ), $\n]
             end,
             Lo ),

  Script1 = apply( Lang, preprocess, [Script] ),

  Script2 = io_lib:format( "~s~n~s~n~s~n~s~n", [LibPath, Assign, Script1, Suffix] ),

  % run script
  {_Port, _ActScript} = apply( FfiType, create_port, [Lang, Script2, Dir] ).



%% listen_port/2
%
listen_port( Port, ActScript ) ->
  listen_port( Port, ActScript, <<>>, #{}, [] ).


%% listen_port/5
%
-spec listen_port( Port, ActScript, LineAcc, ResultAcc, OutAcc ) -> Result
when Port      :: port(),
     ActScript :: iolist(),
     LineAcc   :: binary(),
     ResultAcc :: #{string() => [string()]},
     OutAcc    :: [binary()],
     Result    :: {finished, #{string() => [str()]}, [binary()]}
                | {failed, script_error, {iolist(), [binary()]}}.

listen_port( Port, ActScript, LineAcc, ResultAcc, OutAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      LineAcc1 = <<LineAcc/binary, PartLine/binary>>,
      listen_port( Port, ActScript, LineAcc1, ResultAcc, OutAcc );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->

      % reconstruct line from iolist
      Line = <<LineAcc/binary, PartLine/binary>>,

      case Line of

        % line is a special message
        <<?MSG, AssocStr/binary>> ->

          % parse line
          {ok, Tokens, _} = erl_scan:string( binary_to_list( AssocStr ) ),
          {ok, AssocMap}  = erl_parse:parse_term( Tokens ),

          % continue
          listen_port( Port, ActScript, <<>>, maps:merge( ResultAcc, AssocMap ), OutAcc );

        % line is an ordinary output
        _ ->

          % continue
          listen_port( Port, ActScript, <<>>, ResultAcc, [Line|OutAcc] )

      end;

    % process succeeded
    {Port, {exit_status, 0}} ->

      {finished, ResultAcc, lists:reverse( OutAcc )};

    % process failed
    {Port, {exit_status, _}} ->
      {failed, script_error, {ActScript, lists:reverse( OutAcc )}};

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.


%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

greet_bash_test_() ->

  Script   = "out=\"Hello $person\"",
  Dir      = "/tmp",
  Lo       = [{param, {name, "out", false}, false}],
  Li       = [{param, {name, "person", false}, false}],
  Sign     = {sign, Lo, Li},
  Body     = {forbody, bash, Script},
  Lam      = {lam, 12, "greet", Sign, Body},
  Fa       = #{"person" => [{str, "Jorgen"}]},

  {finished, ResultMap, _} = run( Lam, Fa, Dir ),

  Result = maps:get( "out", ResultMap ),

  ?_assertEqual( [{str, "Hello Jorgen"}], Result ).

-endif.
