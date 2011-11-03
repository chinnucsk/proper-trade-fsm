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
-export([do_connect/0, do_accept/0, a_make_offer/1, b_make_offer/1]).

%% Call in API for the trade_fsm
-export([ask_negotiate/2, accept_negotiate/2, do_offer/2, undo_offer/2,
         are_you_ready/1, not_yet/1, am_ready/1, ack_trans/1,
         ask_commit/1, do_commit/1, notify_cancel/1]).

-record(state, { a_items = [] :: [atom()],
                 b_items = [] :: [atom()] }).
-define(DEFAULT_TIMEOUT, 1000).
-define(LOG(X, Y), io:format(X, Y)).

%% CALL-IN API from the trade_fsm
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

start_controller() ->
    spawn_link(fun() ->
                       register(trade_fsm_proper_controller, self()),
                       loop()
               end),
    ok.

stop_controller() ->
    Ref = make_ref(),
    trade_fsm_proper_controller ! {stop, Ref, self()},
    receive
        Ref ->
            ok
    end.

call_in(_Pid, Msg) ->
    trade_fsm_proper_controller ! {call_in, Msg}.

expect_in(Ty) ->
    R = make_ref(),
    trade_fsm_proper_controller ! {expected, {self(), R}, Ty},
    receive
        {R, Reply} ->
            Reply
    end.

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

loop() ->
    receive
        {stop, Ref, Pid} ->
            Pid ! Ref,
            unregister(trade_fsm_proper_controller),
            ok;
        {expected, Reply, Ty} ->
            ?LOG("Expecting in controller: ~p\n", [Ty]),
            expect(Reply, Ty)
    end,
    loop().

do_connect() ->        
    ?LOG("Asking trade\n", []),
    ok = trade_fsm_controller:trade({trade_fsm_proper, trade_fsm_proper_controller}),
    ?LOG("Expecting ask_negotiate\n", []),
    Reply = expect_in(ask_negotiate),
    ?LOG("Reply from controller: ~p\n", [Reply]),
    Reply.

do_accept() ->
    ?LOG("Accepting in Test Controller\n", []),
    trade_fsm_controller:accept_negotiate(trade_fsm_proper_controller),
    ?LOG("-->Unblocking SUT...\n", []),
    R = case trade_fsm_controller:unblock() of
            {value, ok} ->
                ok;
            Otherwise ->
                Otherwise
        end,
    ?LOG("<--Unblocked!\n", []),
    R.

a_make_offer(Item) ->
    ok = trade_fsm_controller:make_offer(Item),
    expect_in(do_offer).

b_make_offer(Item) ->
    ok = trade_fsm_controller:do_offer(Item).

idle(_S) ->
    [{idle_wait, {call, ?MODULE, do_connect, []}}].

idle_wait(_S) ->
    [{negotiate, {call, ?MODULE, do_accept, []}}].

negotiate(_S) ->
    [{history, {call, ?MODULE, a_make_offer, [item()]}},
     {history, {call, ?MODULE, b_make_offer, [item()]}}].

ready(_S) ->
    [].

wait(_S) ->
    [].

next_state_data(idle, idle_wait, S, _Res, {call, _, do_connect, _}) ->
    S;
next_state_data(idle_wait, negotiate, S, _Res, {call, _, do_accept, _}) ->
    S;
next_state_data(negotiate, negotiate,
                #state { b_items = Items } = S, _, {call, _, a_make_offer, [Item]}) ->
    S#state { b_items = [Item | Items] };
next_state_data(negotiate, negotiate,
                #state { a_items = Items } = S, _, {call, _, b_make_offer, [Item]}) ->
    S#state { a_items = [Item | Items] };
next_state_data(negotiate, negotiate, S, _Res, {call, _, _, _}) ->
    S.

precondition(_, _, _, _) ->
    true.

postcondition(idle, idle_wait, _S, {call, _, do_connect, _}, Res) ->
    ?LOG("Verifying postcondition: ~p: ~p\n", [Res, Res == ok]),
    ok == Res;
postcondition(negotiate, negotiate, _S, {call, _, a_make_offer, [Item]}, Res) ->
    {ok, Item} == Res;
postcondition(negotiate, negotiate, _S, {call, _, b_make_offer, [_Item]}, Res) ->
    ok == Res;
postcondition(_From, _Target, _State, {call, _, _, _}, Res) ->
    Res == ok.

initial_state() ->
    idle.

initial_state_data() ->
    #state{}.


item() ->
    oneof([horse, shotgun, boots, sword, shield, leggings, gstring]).


start() ->
    {ok, _} = trade_fsm_controller:start_link(),
    ok = start_controller(),
    ok.

stop() ->
    ok = trade_fsm_controller:stop(),
    ok = stop_controller(),
    ok.

result_format(History, State, Result) ->
    io:format("History: ~w\nState: ~w\nResult: ~w\n",
              [History, State, Result]).

prop_trade_fsm_correct() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
            ?TRAPEXIT(
            begin
                ok = start(),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                ok = stop(),
                ?WHENFAIL(result_format(History, State, Result),
                          aggregate(zip(proper_fsm:state_names(History),
                                        command_names(Cmds)),
                                    Result =:= ok))
            end)).

qc() ->
    proper:quickcheck(prop_trade_fsm_correct(), 300).

