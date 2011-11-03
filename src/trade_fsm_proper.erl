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

-record(state, { a_items = [] :: [atom()],
                 b_items = [] :: [atom()] }).

-define(LOG(X, Y), io:format(X, Y)).
-define(MOCK, trade_fsm_mock).

%% Call QuickCheck
qc() ->
    proper:quickcheck(prop_trade_fsm_correct(), 300).

%% Generators
item() ->
    oneof([horse, shotgun, boots, sword, shield, leggings, gstring]).



%% Operations we can do
do_connect() ->        
    ?LOG("Asking trade\n", []),
    ok = trade_fsm_controller:trade({?MOCK, ?MOCK}),
    ?LOG("Expecting ask_negotiate\n", []),
    {ok, Reply} = ?MOCK:expect_in(ask_negotiate),
    ?LOG("Reply from controller: ~p\n", [Reply]),
    Reply.

do_accept() ->
    ?LOG("Accepting in Test Controller\n", []),
    trade_fsm_controller:accept_negotiate(?MOCK),
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
    {ok, Reply} = ?MOCK:expect_in(do_offer),
    Reply.

b_make_offer(Item) ->
    ok = trade_fsm_controller:do_offer(Item).

%% QC FSM States
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

%% Initialization
initial_state() ->
    idle.

initial_state_data() ->
    #state{}.

%% State data transitions (symbolic/concrete)
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

%% Precondition: When can a transition happen?
precondition(_, _, _, _) ->
    true.

%% Postcondition, not symbolic, testing properties
postcondition(idle, idle_wait, _S, {call, _, do_connect, _}, Res) ->
    ?LOG("Verifying postcondition: ~p: ~p\n", [Res, Res == ok]),
    ok == Res;
postcondition(negotiate, negotiate, _S, {call, _, a_make_offer, [Item]}, Res) ->
    {ok, Item} == Res;
postcondition(negotiate, negotiate, _S, {call, _, b_make_offer, [_Item]}, Res) ->
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
