%% -*- erlang -*-
%%
%% Effi: Erlang foreign function interface
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( effi_wal_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( effi_wal, [bind_singleton_boolean/2,
				    bind_singleton_string/2,
					bind_boolean_list/2,
				    bind_string_list/2,
				    echo_singleton_boolean/1,
					echo_singleton_string/1,
					echo_boolean_list/1,
					echo_string_list/1,
					prefix/0,
					end_of_transmission/0,
					suffix/0,
					process_script/1,
					get_run_info/1] ).

effi_wal_test_() ->
	{foreach,
	 fun()    -> ok end,
	 fun( _ ) -> ok end,
	 [
	  {"bind_singleton_boolean true",               fun test_bind_singleton_boolean_true/0},
	  {"bind_singleton_boolean false",              fun test_bind_singleton_boolean_false/0},
	  {"bind_singleton_string on empty string",     fun test_bind_singleton_string_empty/0},
	  {"bind_singleton_string on non-empty string", fun test_bind_singleton_string_nonempty/0},
	  {"bind_boolean_list on empty list",           fun test_bind_boolean_list_empty/0},
	  {"bind_boolean_list on non-empty list",       fun test_bind_boolean_list_nonempty/0},
	  {"bind_string_list on empty list",            fun test_bind_string_list_empty/0},
	  {"bind_string_list on non-empty list",        fun test_bind_string_list_nonempty/0},
	  {"echo_singleton_boolean",                    fun test_echo_singleton_boolean/0},
	  {"echo_singleton_string",                     fun test_echo_singleton_string/0},
	  {"echo_boolean_list",                         fun test_echo_boolean_list/0},
	  {"echo_string_list",                          fun test_echo_string_list/0},
	  {"prefix",                                    fun test_prefix/0},
	  {"end_of_transmission",                       fun test_end_of_transmission/0},
	  {"suffix",                                    fun test_suffix/0},
	  {"process_script empty",                      fun test_process_script_empty/0},
	  {"process_script non-empty",                  fun test_process_script_nonempty/0},
	  {"get_run_info",                              fun test_get_run_info/0}
]}.

test_bind_singleton_boolean_true() ->
	?assertEqual( <<"(set (x #t))\n">>, bind_singleton_boolean( <<"x">>, <<"true">> ) ).

test_bind_singleton_boolean_false() ->
	?assertEqual( <<"(set (x #f))\n">>, bind_singleton_boolean( <<"x">>, <<"false">> ) ).

test_bind_singleton_string_empty() ->
	?assertEqual( <<"(set (x \"\"))\n">>, bind_singleton_string( <<"x">>, <<"">> ) ).

test_bind_singleton_string_nonempty() ->
	?assertEqual( <<"(set (x \"blub\"))\n">>, bind_singleton_string( <<"x">>, <<"blub">> ) ).

test_bind_boolean_list_empty() ->
	?assertEqual( <<"(set (l (list)))\n">>, bind_boolean_list( <<"l">>, [] ) ).

test_bind_boolean_list_nonempty() ->
	?assertEqual( <<"(set (l (list #t #f #t)))\n">>, bind_boolean_list( <<"l">>, [<<"true">>, <<"false">>, <<"true">>] ) ).

test_bind_string_list_empty() ->
	?assertEqual( <<"(set (l (list)))\n">>, bind_string_list( <<"l">>, []) ).

test_bind_string_list_nonempty() ->
	?assertEqual( <<"(set (l (list \"bla\" \"blub\")))\n">>, bind_string_list( <<"l">>, [<<"bla">>, <<"blub">>] ) ).

test_echo_singleton_boolean() ->
	?assertEqual( <<"(printf \"<MSG>{\\\"arg_name\\\":\\\"a\\\",\\\"value\\\":\\\"%s\\\"}\\n\" (if a \"true\" \"false\"))\n">>, echo_singleton_boolean( <<"a">> ) ).

test_echo_singleton_string() ->
	?assertEqual( <<"(printf \"<MSG>{\\\"arg_name\\\":\\\"a\\\",\\\"value\\\":\\\"%s\\\"}\\n\" a)\n">>, echo_singleton_string( <<"a">> ) ).

test_echo_boolean_list() ->
	?assertEqual( <<"(let ((f (lambda (x) (if x \"true\" \"false\"))))\n  (printf \"<MSG>{\\\"arg_name\\\":\\\"l\\\",\\\"value\\\":[\")\n  (if (length l)\n     (let ((hd (first l)) (tl (rest l))) (printf \"\\\"%s\\\"\" (f hd)) (map (lambda (x) (printf \",\\\"%s\\\"\" (f x))) tl)))\n  (printf \"]}\\n\"))\n">>, echo_boolean_list( <<"l">> ) ).

test_echo_string_list() ->
	?assertEqual( <<"(printf \"<MSG>{\\\"arg_name\\\":\\\"l\\\",\\\"value\\\":[\")\n(if (length l)\n   (let ((hd (first l)) (tl (rest l))) (printf \"\\\"%s\\\"\" hd) (map (lambda (x) (printf \",\\\"%s\\\"\" x)) tl)))\n(printf \"]}\\n\")\n">>, echo_string_list( <<"l">> ) ).

test_prefix() ->
	?assertEqual( <<>>, prefix() ).

test_end_of_transmission() ->
	?assertEqual( <<"(print \"<EOT>\")\n">>, end_of_transmission() ).

test_suffix() ->
	?assertEqual( <<>>, suffix() ).

test_process_script_empty() ->
	?assertEqual( <<>>, process_script( <<>> ) ).

test_process_script_nonempty() ->
	?assertEqual( <<"blub">>, process_script( <<"blub">> ) ).

test_get_run_info() ->
	?assertEqual( [], get_run_info( #{} ) ).
