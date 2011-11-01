-module(trade_fsm_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(proper_fsm).

%% Proper FSM API
-export([qc/0]).
-export([idle/1, idle_wait/1, negotiate/1, ready/1, wait/1,
         next_state_data/5,
         precondition/4,
         postcondition/5,
         initial_state/0, initial_state_data/0]).

%% Calls the Test system uses to carry out possible events
-export([do_connect/0]).

%% Call in API for the trade_fsm
-export([ask_negotiate/2, accept_negotiate/2, do_offer/2, undo_offer/2,
         are_you_ready/1, not_yet/1, am_ready/1, ack_trans/1,
         ask_commit/1, do_commit/1, notify_cancel/1]).

-record(state, {}).
-define(DEFAULT_TIMEOUT, 5000).

%% CALL-IN API from the trade_fsm
ask_negotiate(Pid, Myself) ->
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

call_in(_Pid, _Msg) ->
    todo.

expect(Msg) ->
    expect(Msg, ?DEFAULT_TIMEOUT).

expect(Msg, Timeout) ->
    receive
        Msg ->
            ok;
        Other ->
            {error, {unexpected, Other}}
    after Timeout ->
            {error, timeout}
    end.

do_connect() ->        
    ok = trade_fsm_controller:trade({trade_fsm_proper, trade_mock}),
    expect({ask_negotiate, '_', trade_fsm_proper}).

idle(_S) ->
    [{idle_wait, {call, ?MODULE, do_connect, []}}].

idle_wait(_S) ->
    todo.

negotiate(_S) ->
    todo.

ready(_S) ->
    todo.

wait(_S) ->
    todo.

next_state_data(idle, idle_wait, S, _Res, {call, _, do_connect, _}) ->
    S.

precondition(_, _, _, _) ->
    true.

postcondition(idle, idle_wait, _S, {call, _, do_connect, _}, Res) ->
    Res == ok;
postcondition(_, _, _, _, _) ->
    false.

initial_state() ->
    idle.

initial_state_data() ->
    #state{}.


start() ->
    {ok, _} = trade_fsm_controller:start_link().

stop() ->
    ok = trade_fsm_controller:stop().

prop_trade_fsm_correct() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
            ?TRAPEXIT(
            begin
                ok = start(),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                ok = stop(),
                ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                    [History, State, Result]),
                          aggregate(zip(proper_fsm:state_names(History),
                                        command_names(Cmds)),
                                    true))
            end)).

qc() ->
    proper:quickcheck(prop_trade_fsm_correct(), 5).

