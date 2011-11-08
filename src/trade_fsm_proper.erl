-module(trade_fsm_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(proper_fsm).

%% Proper FSM API
-export([qc/0]).
-export([idle/1, idle_wait/1, idle_wait_b/1, negotiate/1, ready/1, wait/1,
         next_state_data/5,
         precondition/4,
         postcondition/5,
         weight/3,
         initial_state/0, initial_state_data/0]).

%% Calls the Test system uses to carry out possible events
-export([a_trade/0, a_do_accept/0, b_trade/0, b_do_accept/0,

         a_make_offer/1, b_make_offer/1,
         a_retract_offer/1, b_retract_offer/1,
         a_ready/0, b_ready/0, a_not_yet/0, b_not_yet/0,
         expect_a_ask_negotiate/0, expect_a_ask_negotiate_unblock/0,

         commit_transaction/0]).

-record(state, { a_items = [] :: [atom()],
                 b_items = [] :: [atom()],
                 a_blocked = false :: boolean() }).

-define(LOG(X, Y), io:format(X, Y)).
-define(MOCK, trade_fsm_mock).

%% Call QuickCheck
qc() ->
    proper:quickcheck(prop_trade_fsm_correct(), 300).

%% Generators
item() ->
    oneof([horse, shotgun, boots, sword, shield, leggings,
           gstring, book, leash, collar, japanese_girl,
           railgun]).

%% Operations we can do
a_trade() ->        
    trade_fsm_controller:trade({?MOCK, ?MOCK}).

b_trade() ->
    trade_fsm_controller:ask_negotiate(?MOCK, ?MOCK).

expect_a_ask_negotiate() ->
    {ok, Reply} = ?MOCK:expect_in(ask_negotiate),
    Reply.
    
expect_a_ask_negotiate_unblock() ->
    R1 = expect_a_ask_negotiate(),
    R2 = unblock(),
    {R1, R2}.

unblock() ->
    ?LOG("-->Unblocking SUT...\n", []),
    R = case trade_fsm_controller:unblock() of
            {value, ok} ->
                ok;
            Otherwise ->
                Otherwise
        end,
    ?LOG("<--Unblocked!\n", []),
    R.
    
b_do_accept() ->
    trade_fsm_controller:accept_negotiate(?MOCK),
    unblock().

a_do_accept() ->
    trade_fsm_controller:accept_trade().
            
a_make_offer(Item) ->
    ok = trade_fsm_controller:make_offer(Item),
    {ok, Reply} = ?MOCK:expect_in({do_offer, Item}),
    Reply.

b_make_offer(Item) ->
    ok = trade_fsm_controller:do_offer(Item).

a_retract_offer(Item) ->
    ok = trade_fsm_controller:retract_offer(Item),
    {ok, Reply} = ?MOCK:expect_in({undo_offer, Item}),
    Reply.

b_retract_offer(Item) ->
    trade_fsm_controller:undo_offer(Item).
       
a_ready() ->
    trade_fsm_controller:ready(),
    ok.

a_not_yet() ->
    {ok, Reply} = ?MOCK:expect_in(not_yet),
    Reply.

b_not_yet() ->
    trade_fsm_controller:not_yet().

b_ready() ->
    trade_fsm_controller:are_you_ready().

commit_transaction() ->
    ok.

%% Transitions when A is allowed to manipulate items
a_item_manipulation(S) ->
    [{history, {call, ?MODULE, a_make_offer, [item()]}},
     {history, {call, ?MODULE, a_retract_offer, [elements(S#state.a_items)]}}].

%% Transitions when B is allowed to manipulate items
b_item_manipulation(S) ->
    %% [{history, {call, ?MODULE, b_make_offer_unblock, [item()]}},
    %%  {history, {call, ?MODULE, b_retract_offer_unblock, [elements(S#state.b_items)]}}] ++
    [{history, {call, ?MODULE, b_make_offer, [item()]}},
     {history, {call, ?MODULE, b_retract_offer, [elements(S#state.b_items)]}}].

%% QC FSM States

%% The state when the test FSM is idle
idle(_S) ->
    [{history, {call, ?MODULE, a_trade, []}}] ++
    [{idle_wait, {call, ?MODULE, expect_a_ask_negotiate, []}}] ++
    [{idle_wait_b, {call, ?MODULE, b_trade, []}}].

%% Special idle_wait state for the cross-case
idle_wait_b(_S) ->
    [{negotiate, {call, ?MODULE, expect_a_ask_negotiate_unblock, []}},
     {negotiate, {call, ?MODULE, a_do_accept, []}}].

%% "Normal" idle_wait state
idle_wait(_S) ->
    [{negotiate, {call, ?MODULE, b_do_accept, []}}] ++
    [{negotiate, {call, ?MODULE, a_do_accept, []}}].

%% Important thing to figure out:
%%   How can we leave the negotiate state? We can get to wait, but how
%%   about the ready state directly?
%%  IMPORTANT: Check sync/async rules!
negotiate(S) ->
    a_item_manipulation(S) ++
    b_item_manipulation(S) ++ [].
%%        [{history, {call, ?MODULE, a_ready, []}},
%%         {history, {call, ?MODULE, b_not_yet, []}},
%%         {wait,    {call, ?MODULE, b_ready, []}}].


%% Consider splitting this into: a waits and b waits. It might be easier to
%% model than keeping a state on the wait/block state of a.
wait(S) ->
    %% Needs states here where 'a' is ready!
    %% Needs a precondition on a_not_yet/0 it must fire based on 'a's state
    [{history,    {call, ?MODULE, expect_a_not_yet, []}},
     {ready,      {call, ?MODULE, expect_a_am_ready, []}},
     {history,    {call, ?MODULE, a_ready, []}},
     {ready,      {call, ?MODULE, b_am_ready, []}}] ++
        a_item_manipulation(S).

ready(_S) ->
    [{history, {call, ?MODULE, commit_transaction, []}}].


%% Initialization
initial_state() ->
    idle.

initial_state_data() ->
    #state{}.

%% State data transitions (symbolic/concrete)
next_state_data(_, _, S, _Res, {call, _, a_trade, _}) ->
    S#state { a_blocked = true };
next_state_data(idle, idle_wait, S, _Res, {call, _, trade, _}) ->
    S;
next_state_data(idle_wait, negotiate, S, _Res, {call, _, do_accept, _}) ->
    S;
next_state_data(negotiate, negotiate,
                #state { b_items = Items } = S, _, {call, _, a_make_offer, [Item]}) ->
    S#state { a_items = [Item | Items] };
next_state_data(negotiate, negotiate,
                #state { a_items = Items } = S, _, {call, _, b_make_offer, [Item]}) ->
    S#state { b_items = [Item | Items], a_blocked = false };
next_state_data(_, _,
                #state { a_items = Items } = S, _, {call, _, a_retract_offer, [Item]}) ->
    S#state { a_items = lists:delete(Item, Items) };
next_state_data(_, _,
                #state { b_items = Items } = S, _, {call, _, b_retract_offer, [Item]}) ->
    S#state { b_items = lists:delete(Item, Items), a_blocked = false };
next_state_data(_, _, S, _Res, {call, _, a_ready, _}) ->
    S#state { a_blocked = true };
next_state_data(negotiate, ready, S, _Res, {call, _, _, _}) ->
    S;
next_state_data(ready, ready, S, _Res, {call, _, commit_transaction, _}) ->
    S;
next_state_data(negotiate, negotiate, S, _Res, {call, _, _, _}) ->
    S;
next_state_data(_, _, S, _Res, {call, _, expect_a_ask_negotiate_unblock, _}) ->
    S#state { a_blocked = false };
next_state_data(_, _, S, _, _) ->
    S.


%% Precondition: When can a transition happen?

%% A can only accept when it is not blocked
precondition(_, _, S, {call, _, a_do_accept, _}) ->
    not S#state.a_blocked;
%% A may only connect when it is not blocked
precondition(idle, idle, S, {call, _, a_trade, _}) ->
    not S#state.a_blocked;
%% May only expect ask_negotiate if A is blocked
precondition(_, _, S, {call, _, expect_a_ask_negotiate, _}) ->
    S#state.a_blocked;
precondition(_, _, S, {call, _, expect_a_ask_negotiate_unblock, _}) ->
    S#state.a_blocked;
precondition(negotiate, negotiate, #state { a_blocked = false}, {call, _, a_make_offer, _}) -> true;
precondition(negotiate, negotiate, #state { a_blocked = true}, {call, _, a_make_offer,_}) -> false;
precondition(negotiate, negotiate, #state { a_blocked = false}, {call, _, a_retract_offer, _}) -> true;
precondition(negotiate, negotiate, #state { a_blocked = true}, {call, _, a_retract_offer,_}) -> false;
precondition(negotiate, negotiate, #state { a_blocked = false}, {call, _, a_ready, _}) -> true;
precondition(negotiate, negotiate, #state { a_blocked = true}, {call, _, a_ready, _}) -> false;
precondition(negotiate, negotiate, #state { a_blocked = false}, {call, _, b_not_yet, _}) -> false;
precondition(negotiate, negotiate, #state { a_blocked = true}, {call, _, b_not_yet, _}) -> true;
precondition(_, _, _, _) ->
    true.

%% Postcondition, not symbolic, testing properties
postcondition(idle, idle_wait, _S, {call, _, trade, _}, Res) ->
    ?LOG("Verifying postcondition: ~p: ~p\n", [Res, Res == ok]),
    ok == Res;
postcondition(negotiate, negotiate, _S, {call, _, a_make_offer, [Item]}, Res) ->
    {ok, Item} == Res;
postcondition(negotiate, negotiate, _S, {call, _, b_make_offer, [_Item]}, Res) ->
    ok == Res;
postcondition(_, _, _S, {call, _, a_retract_offer, [_Item]}, Res) ->
    ok == Res;
postcondition(_, _, _S, {call, _, b_retract_offer, [_Item]}, Res) ->
    ok == Res;
postcondition(_From, _Target, _State, {call, _, _, _}, Res) ->
    Res == ok.

%% Lifecycle
start() ->
    {ok, _} = trade_fsm_controller:start_link(),
    ok = ?MOCK:start_link(),
    ok.

stop() ->
    ok = trade_fsm_controller:stop(),
    ok = ?MOCK:stop(),
    ok.


weight(_, _, {call, _, commit_transaction, _}) -> 1;
weight(_, _, _) -> 3.
    
%% Property test
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
