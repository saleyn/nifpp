-module(nifpptest).
-export([invoke_nif/1, start/0]).
-on_load(init/0).
-include_lib("eunit/include/eunit.hrl").

init() ->
    ok = erlang:load_nif("./nifpptest", 0).

invoke_nif(_X) ->
    exit(nif_library_not_loaded).


start() ->
    test(),
    benchmark(),
    halt(0).

do_times(_F, 0) ->
    ok;
do_times(F, N) ->
    F(),
    do_times(F,N-1).

benchmark() ->
    Reps = 3000,
    InnerReps = 20000,
    Input = {{1,2},3},
    Self = self(),
    % force load modules
    tuple_twiddle_c:twiddle(Input),
    tuple_twiddle_cpp:twiddle(Input),
    erlang:garbage_collect(),
    Tests = [
        {cpp_tup_orig,  fun() -> tuple_twiddle_cpp:twiddle(Input)        end},
        {cpp_tup_apply, fun() -> tuple_twiddle_cpp:twiddle_tuple1(Input) end},
        {cpp_tup_array, fun() -> tuple_twiddle_cpp:twiddle_tuple2(Input) end},
        {c,             fun() -> tuple_twiddle_c:twiddle(Input)          end}
    ],
    Procs = [{Name, spawn_link(fun() -> Self ! {Name, test_avg(fun() -> do_times(F, InnerReps) end, Reps)} end)} || {Name, F} <- Tests],

    io:format("~-13s |   Min |   Max | Median |   Avg   |   95%   |   99%   | (all times in µs)\n", ["Benchmark"]),
    io:format("--------------+-------+-------+--------+---------+---------+---------+\n", []),

    List0 = [receive {N, Stats} -> {atom_to_list(N), Stats} after 60000 -> error(N) end || {N, _Pid} <- Procs],

    List  = lists:sort(fun({_, #{n99 := M1}}, {_, #{n99 := M2}}) -> M1 < M2 end, List0),
    {MinName, #{n99 := MinMed}} = hd(List),
    [print(N, S, MinName, MinMed) || {N, S} <- List],
    ok.

print(Name, #{min := Min, max := Max, med := Med, avg := Avg, n95 := N95, n99 := N99}, MinName, MinMed) ->
    Pcnt = N99 * 100.0 / MinMed - 100,
    io:format("~-13s | ~5w | ~5w | ~6w | ~7w | ~7w | ~7w |~s~-7s~s\n",
        [Name, Min, Max, Med, Avg, N95, N99,
         if Name == MinName -> ""; Pcnt >= 0 -> " +"; true -> " " end,
         if Name == MinName -> ""; true -> float_to_list(Pcnt, [{decimals, 2}]) ++ "%" end,
         case Name of MinName -> ""; _ -> " of " ++ MinName end
        ]).

test_avg(F, N) when N > 0 ->
    L = test_loop(F, N, []),
    List = lists:sort(L),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round(Length / 2), List),
    N95 = lists:nth(round(Length * 0.95), List),
    N99 = lists:nth(round(Length * 0.99), List),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    #{min => Min, max => Max, med => Med, avg => Avg, n95 => N95, n99 => N99}.

test_loop(_F, 0, List) ->
    List;
test_loop(F, N, List) ->
    {T, _Result} = timer:tc(F),
    test_loop(F, N - 1, [T|List]).

atom_test_() ->
    [
        ?_assertEqual( abcabc, invoke_nif({atom2, abc})),
        ?_assertEqual( abc123abc123, invoke_nif({atom2, abc123})),
        ?_assertError( badarg, invoke_nif({atom2, 123}))
        ].

string_test_() ->
    [
        ?_assertEqual( "abcabc", invoke_nif({string2, "abc"})),
        ?_assertEqual( "abc123abc123", invoke_nif({string2, "abc123"})),
        ?_assertEqual( "abcabc", invoke_nif({string2, <<"abc">>})),
        ?_assertEqual( "abc123abc123", invoke_nif({string2, <<"abc123">>})),
        ?_assertError( badarg, invoke_nif({string2, 123}))
        ].

double_test_() ->
    [
        ?_assertEqual( 246.0, invoke_nif({double2, 123.0})),
        ?_assertEqual( 1000.0, invoke_nif({double2, 500.0})),
        ?_assertError( badarg, invoke_nif({double2, abc}))
        ].

int_test_() ->
    [
        ?_assertEqual( 246, invoke_nif({int2, 123})),
        ?_assertEqual( 1000, invoke_nif({int2, 500})),
        ?_assertError( badarg, invoke_nif({int2, abc}))
        ].

uint_test_() ->
    [
        ?_assertEqual( 246, invoke_nif({uint2, 123})),
        ?_assertEqual( 1000, invoke_nif({uint2, 500})),
        ?_assertError( badarg, invoke_nif({uint2, abc}))
        ].

long_test_() ->
    [
        ?_assertEqual( 246, invoke_nif({long2, 123})),
        ?_assertEqual( 1000, invoke_nif({long2, 500})),
        ?_assertError( badarg, invoke_nif({long2, abc}))
        ].

bool_test_() ->
    [
        ?_assertEqual( true, invoke_nif({booln, false})),
        ?_assertEqual( false, invoke_nif({booln, true}))
    ].

pid_test_() ->
    [
        ?_assertEqual( self(), invoke_nif({pidcp, self()}))
    ].

tuple_test_() ->
    [
        ?_assertEqual( {246, "abcabc", {xyzxyz, 912}, 1000.0},
                invoke_nif({tuple2a, {123, "abc", {xyz, 456}, 500.0}})),
        ?_assertEqual( {246, "abcabc", 1000.0},
                invoke_nif({tuple2b, {123, "abc", 500.0}})),
        ?_assertEqual( {246, "abcabc", {xyzxyz, 912}, 1000.0},
                invoke_nif({tuple2c, {123, "abc", {xyz, 456}, 500.0}})),
        ?_assertNot(
                invoke_nif({tuple2d, {"abc", 1, 500.0}}))
%%        ?_assertEqual( {246, "abcabc", 1000.0},
%%                invoke_nif({tuple2d, {123, "abc", 500.0}})),
%%        ?_assertEqual( {246, "abcabc", {xyzxyz, 912}, 1000.0},
%%                invoke_nif({tuple2e, {123, "abc", {xyz, 456}, 500.0}}))
                ].


list_test_() ->
    [
        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2aa, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2ab, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2ac, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,4,5],           lists:sort(invoke_nif({list2ad, [1,2,3,5,4]}))),
        ?_assertEqual([1,1,2,2,3,3,4,4,5,5], lists:sort(invoke_nif({list2ae, [1,2,3,5,4]}))),
        ?_assertEqual([1,2,3,4,5],           lists:sort(invoke_nif({list2af, [1,2,3,5,4]}))),

        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2ba, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2bb, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,5,4,4,5,3,2,1], invoke_nif({list2bc, [1,2,3,5,4]})),
        ?_assertEqual([1,2,3,4,5],           lists:sort(invoke_nif({list2bd, [1,2,3,5,4]}))),
        ?_assertEqual([1,1,2,2,3,3,4,4,5,5], lists:sort(invoke_nif({list2be, [1,2,3,5,4]}))),
        ?_assertEqual([1,2,3,4,5],           lists:sort(invoke_nif({list2bf, [1,2,3,5,4]}))),

        ?_assertEqual(["1","2","3","5","4","4","5","3","2","1"], invoke_nif({list2ba, ["1","2","3","5","4"]})),
        ?_assertEqual(["1","2","3","5","4","4","5","3","2","1"], invoke_nif({list2bb, ["1","2","3","5","4"]})),
        ?_assertEqual(["1","2","3","5","4","4","5","3","2","1"], invoke_nif({list2bc, ["1","2","3","5","4"]})),
        ?_assertEqual(["1","2","3","4","5"],                     lists:sort(invoke_nif({list2bd, ["1","2","3","5","4"]}))),
        ?_assertEqual(["1","1","2","2","3","3","4","4","5","5"], lists:sort(invoke_nif({list2be, ["1","2","3","5","4"]}))),
        ?_assertEqual(["1","2","3","4","5"],                     lists:sort(invoke_nif({list2bf, ["1","2","3","5","4"]}))),

        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2ba, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2bb, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2bc, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}],                     lists:sort(invoke_nif({list2bd, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]}))),
        ?_assertEqual([{1,"1"},{1,"1"},{2,"2"},{2,"2"},{3,"3"},{3,"3"},{9,"5"},{9,"5"},{16,"4"},{16,"4"}], lists:sort(invoke_nif({list2be, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]}))),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}],                     lists:sort(invoke_nif({list2bf, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]}))),

        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2ca, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2cb, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"},{16,"4"},{9,"5"},{3,"3"},{2,"2"},{1,"1"}], invoke_nif({list2cc, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})),
        ?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}],                     lists:sort(invoke_nif({list2cd, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]}))),
        ?_assertEqual([{1,"1"},{1,"1"},{2,"2"},{2,"2"},{3,"3"},{3,"3"},{9,"5"},{9,"5"},{16,"4"},{16,"4"}], lists:sort(invoke_nif({list2ce, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})))
        %?_assertEqual([{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}],                     lists:sort(invoke_nif({list2cf, [{1,"1"},{2,"2"},{3,"3"},{9,"5"},{16,"4"}]})))

        ].

stdarray_test_() ->
    [
        ?_assertEqual([1,2,4,5,4], invoke_nif({stdarray_inc2, [1,2,3,5,4]})),
        ?_assertError(badarg, invoke_nif({stdarray_inc2, [1,2,3,4,5,6]})), % too long
        ?_assertError(badarg, invoke_nif({stdarray_inc2, [1,2,3,4]})), % too short
        ?_assertEqual([1,2,5,5,4], invoke_nif({stdarray_cp32, [1,2,3,5,4]}))
    ].

map_test_() ->
    [
        ?_assertEqual(#{123 => abc,456 => def,789 => pqr}, invoke_nif({mapflipaa, #{abc=>123, def=>456, pqr=>789}})),
        ?_assertEqual(#{123 => abc,456 => def,789 => pqr}, invoke_nif({mapflipab, #{abc=>123, def=>456, pqr=>789}})),
        ?_assertEqual(#{123 => abc,456 => def,789 => pqr}, invoke_nif({mapflipba, #{abc=>123, def=>456, pqr=>789}})),
        ?_assertEqual(#{123 => abc,456 => def,789 => pqr}, invoke_nif({mapflipbb, #{abc=>123, def=>456, pqr=>789}})),
        ?_assertEqual( #{a => 1,b => "b",c => #{a1 => 1,b1 => 2}}, invoke_nif({nestedmap, #{}}))

%% these are no good, they depend on iteration order which is undefined.
%%         ?_assertEqual(#{123 => abc,456 => def}, invoke_nif({mapflipaa, #{abc=>123, def=>456, def=>123}})),
%%         ?_assertEqual(#{123 => abc,456 => def}, invoke_nif({mapflipab, #{abc=>123, def=>456, pqr=>123}})),
%%         ?_assertEqual(#{123 => abc,456 => def}, invoke_nif({mapflipba, #{abc=>123, def=>456, def=>123}})),
%%         ?_assertEqual(#{123 => abc,456 => def}, invoke_nif({mapflipbb, #{abc=>123, def=>456, pqr=>123}}))


].

intres_test_() ->
    [
        fun() ->
            Res = invoke_nif({makeresint, 22}),
            ?assertEqual(23, invoke_nif({incresint, Res})),
            ?assertEqual(24, invoke_nif({incresint, Res})),
            ?assertEqual(25, invoke_nif({incresint, Res})),
            ?assertEqual(24, invoke_nif({decresint, Res})),
            ?assertEqual(23, invoke_nif({decresint, Res})),
            ?assertEqual(22, invoke_nif({decresint, Res}))
        end,
        fun() ->
            Res = invoke_nif({makeresint_ext, 22}),
            ?assertEqual(23, invoke_nif({incresint, Res})),
            ?assertEqual(24, invoke_nif({incresint, Res})),
            ?assertEqual(25, invoke_nif({incresint, Res})),
            ?assertEqual(24, invoke_nif({decresint, Res})),
            ?assertEqual(23, invoke_nif({decresint, Res})),
            ?assertEqual(22, invoke_nif({decresint, Res}))
        end
    ].

tracetyperes_test_() ->
    Temp = fun() ->
        Rs = [ invoke_nif({tracetype_create,[]}) || _ <- lists:seq(1,10) ],
        ?assertEqual({10,0,0,0}, invoke_nif({tracetype_getcnts,[]})),
        erlang:garbage_collect(),
        ?assertEqual({10,0,0,0}, invoke_nif({tracetype_getcnts,[]})),
        Rs % Rs gets collected above if this is not present
    end,
    [
        ?_assertEqual(ok, invoke_nif({tracetype_reset,[]})),
        ?_assertEqual({0,0,0,0}, invoke_nif({tracetype_getcnts,[]})),
        Temp,
        ?_test(erlang:garbage_collect()),
        ?_assertEqual({10,10,0,0}, invoke_nif({tracetype_getcnts,[]}))
    ].

tracetyperes_mon_test_() ->
    Pid = spawn(fun() -> timer:sleep(60000) end),
    Temp = fun() ->
        Rs = [ invoke_nif({tracetype_mon_create,Pid}) || _ <- lists:seq(1,10) ],
        ?assertEqual({10,0,10,0}, invoke_nif({tracetype_getcnts,[]})),
        erlang:exit(Pid, kill),
        erlang:garbage_collect(),
        ?assertEqual({10,0,10,0}, invoke_nif({tracetype_getcnts,[]})),
        % The sleep forces a context switch which allows the monitor to
        % propagate the events of the Pid's death
        timer:sleep(1),
        ?assertEqual({10,0,10,10}, invoke_nif({tracetype_getcnts,[]})),
        Rs % Rs gets collected above if this is not present
    end,
    [
        ?_assertEqual(ok, invoke_nif({tracetype_reset,[]})),
        ?_assertEqual({0,0,0,0}, invoke_nif({tracetype_getcnts,[]})),
        Temp,
        ?_test(erlang:garbage_collect()),
        ?_assertEqual({10,10,10,10}, invoke_nif({tracetype_getcnts,[]}))
    ].

bin_test_() ->
    [
    ?_assertEqual(<<"boatsboats">>, invoke_nif({bin2, <<"boats">>})),
    fun() ->
        ?assertEqual(ok, invoke_nif({binary_release_counter_reset,[]})),
        ?assertEqual(0, invoke_nif({binary_release_counter_get,[]})),
        ?assertEqual(ok, invoke_nif({bina,[]})),
        ?assertEqual(3, invoke_nif({binary_release_counter_get,[]}))
    end
    ].

exception_test_() ->
    [
        ?_assertError(badarg, invoke_nif({badarg, []})),
        ?_assertError({error, "exception"}, invoke_nif({raise_exception, nil})),
        ?_assertError(bad,    invoke_nif({raise_exception, bad}))
    ].


