-module(trade_fsm_mock).

%% Public API
-export([start_link/0, stop/0, expect_in/1]).

%% Call in API for the trade_fsm
-export([ask_negotiate/2, accept_negotiate/2, do_offer/2, undo_offer/2,
         are_you_ready/1, not_yet/1, am_ready/1, ack_trans/1,
         ask_commit/1, do_commit/1, notify_cancel/1]).

-define(DEFAULT_TIMEOUT, 1000).
-define(LOG(X, Y), io:format(X, Y)).

%% Public API for start/stop live
%% ----------------------------------------------------------------------
start_link() ->
    spawn_link(fun() ->
                       register(trade_fsm_mock, self()),
                       loop()
               end),
    ok.

%% Use a monitor here as well
stop() ->
    Ref = make_ref(),
    trade_fsm_mock ! {stop, Ref, self()},
    receive
        Ref ->
            ok
    end.

expect_in(Ty) ->
    Mref = erlang:monitor(process, trade_fsm_mock),
    trade_fsm_mock ! {expected, {self(), Mref}, Ty},
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, Reason} ->
            {error, Reason}
    end.


%% CALL-IN API from the trade_fsm
%% ----------------------------------------------------------------------
ask_negotiate(Pid, Myself) ->
    ?LOG("Calling trade_fsm_proper:ask_negotiate/2\n", []),
    call_in(Pid, {ask_negotiate, Myself}).

accept_negotiate(Pid, Myself) ->
    call_in(Pid, {accept_negotiate, Myself}).

do_offer(Pid, Item) ->
    call_in(Pid, {do_offer, Item}).

undo_offer(Pid, Item) ->
    call_in(Pid, {undo_offer, Item}).

are_you_ready(Pid) ->
    call_in(Pid, are_you_ready).

not_yet(Pid) ->
    call_in(Pid, not_yet).

am_ready(Pid) ->
    call_in(Pid, 'ready!').

ack_trans(Pid) ->
    call_in(Pid, ack_trans).


ask_commit(Pid) ->
    call_in(Pid, ask_commit).

do_commit(Pid) ->
    call_in(Pid, do_commit).

notify_cancel(Pid) ->
    call_in(Pid, cancel).

%% Internal workings
%% ----------------------------------------------------------------------
call_in(_Pid, Msg) ->
    trade_fsm_mock ! {call_in, Msg}.

expect({Reply, Tag}, Ty) ->
    receive
        {call_in, Msg} ->
            Reply ! {Tag, handle_expect(Ty, Msg)}
    after ?DEFAULT_TIMEOUT ->
            Reply ! {Tag, {error, timeout}}
    end.

handle_expect(ask_negotiate, {ask_negotiate, _}) ->
    ok;
handle_expect(do_offer, {do_offer, Item}) ->
    {ok, Item};
handle_expect(Ty, M) ->
    {error, {unexpected, [{ty, Ty}, {msg, M}]}}.

%% @doc Main loop of the mocking code
%% @end
loop() ->
    receive
        {stop, Ref, Pid} ->
            Pid ! Ref,
            unregister(trade_fsm_mock),
            ok;
        {expected, Reply, Ty} ->
            ?LOG("Expecting in controller: ~p\n", [Ty]),
            expect(Reply, Ty)
    end,
    loop().
