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
                 | {failed, atom(), _}.

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

-export( [check_run/4] ).

%% ------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------


%% check_run/4
%
-spec check_run( Lam, Fa, R, Dir ) -> result()
when Lam :: lam(),
     Fa  :: #{string() => [str()]},
     R   :: pos_integer(),
     Dir :: string().

check_run( Lam, Fa, R, Dir ) ->

  % take start time
  Tstart = trunc( os:system_time()/1000000 ),

  {lam, _Line, _LamName, Sign, _Body} = Lam,
  {sign, Lo, Li} = Sign,

  % check pre-conditions
  PreMissingLst = check_if_file( Li, Fa, Dir ),

  case PreMissingLst of
    [_|_] -> {failed, R, precond, PreMissingLst};
    []    ->

      % run
      case run( Lam, Fa, Dir ) of
        {failed, script_error, Data} -> {failed, R, script_error, Data};
        {finished, RMap, Out}        ->

          % check post-conditions
          PostMissingLst = check_if_file( Lo, RMap, Dir ),

          case PostMissingLst of
            [_|_] -> {failed, R, postcond, PostMissingLst};
            []    ->

              % take duration
              Tdur = trunc( os:system_time()/1000000 )-Tstart,

              % generate summary
              {finished, get_summary( Lam, Fa, R, RMap, Out, Tstart, Tdur )}
          end
      end
  end.


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


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------


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

-spec run( Lam, Fa, Dir ) -> result()
when Lam :: lam(),
     Fa  :: #{string() => [str()]},
     Dir :: string().


%% run/3
%
run( Lam, Fa, Dir ) ->

  % create port
  {Port, ActScript} = create_port( Lam, Fa, Dir ),

  % receive result
  listen_port( Port, ActScript ).


%% create_port/3
%
-spec create_port( Lam, Fa, Dir ) -> {port(), string()}
when Lam :: lam(),
     Fa  :: #{string() => [str()]},
     Dir :: string().

create_port( Lam, Fa, Dir )
when is_tuple( Lam ), is_map( Fa ), is_list( Dir ) ->

  {lam, _Line, _LamName, Sign, Body} = Lam,
  {forbody, Lang, Script} = Body,
  {sign, Lo, Li} = Sign,

  % get Foreign Function Interface type
  FfiType = apply( Lang, ffi_type, [] ),

  % collect assignments
  Prefix = lists:map(
             fun( {param, {name, N, _Pf}, Pl} ) ->
               X = maps:get( N, Fa ),
               X1 = [S ||{str, S} <- X],
               [apply( Lang, assignment, [N, Pl, X1] ),$\n]
             end,
             Li ),

  % collect dismissals
  Suffix = lists:map(
             fun( {param, {name, N, _Pf}, Pl} ) ->
               [apply( Lang, dismissal, [N, Pl] ), $\n]
             end,
             Lo ),

  Script1 = string:join( [Prefix, Script, Suffix], "\n" ),

  % run script
  {_Port, _ActScript} = apply( FfiType, create_port, [Lang, Script1, Dir] ).



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
     Result    :: result().

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
