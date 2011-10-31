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

-record(state, {}).

idle(_S) ->
    todo.

idle_wait(_S) ->
    todo.

negotiate(_S) ->
    todo.

ready(_S) ->
    todo.

wait(_S) ->
    todo.

next_state_data(_, _, _, _, _) ->
    todo.

precondition(_, _, _, _) ->
    todo.

postcondition(_, _, _, _, _) ->
    todo.

initial_state() ->
    idle.

initial_state_data() ->
    #state{}.


start() ->
    ok.

stop() ->
    ok.

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

