-module(selecttest).

-include_lib("eunit/include/eunit.hrl").

-export([create_timer/0, create_timer/1, delete_timer/1, set_timer/2, read_timer/1]).

-on_load(init/0).

-ifdef(EUNIT).
-export([start/0]).
-endif.

init() ->
  ok = erlang:load_nif("./selecttest", 0).

create_timer() ->
  exit(nif_library_not_loaded).

create_timer(TimeoutMS) when is_integer(TimeoutMS), TimeoutMS >= 0 ->
  exit(nif_library_not_loaded).

delete_timer(Timer) when is_reference(Timer) ->
  exit(nif_library_not_loaded).

set_timer(Timer, TimeoutMS) when is_reference(Timer), is_integer(TimeoutMS), TimeoutMS >= 0 ->
  exit(nif_library_not_loaded).

read_timer(Timer) when is_reference(Timer) ->
  exit(nif_library_not_loaded).

-ifdef(EUNIT).

start() ->
  test().

timer_test_() ->
  {setup,
    fun() -> {ok, T} = create_timer(), T end,
    fun(T) -> delete_timer(T) end,
    {with, [
      fun(T) ->
        {ok, T} = set_timer(T, 1),
        ?assertMatch({select, timer}, receive M -> M after 1000 -> timeout end),
        ?assertEqual({ok, T}, set_timer(T, 1)),
        ?assertMatch({select, timer}, receive M -> M after 1000 -> timeout end)
      end
    ]}
  }.

-endif.
