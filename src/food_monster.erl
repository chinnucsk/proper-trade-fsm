-module(food_monster).
-behaviour(gen_fsm).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(proper_fsm).

-export([initial_state/0, initial_state_data/0,
         next_state_data/5,
         postcondition/5,
         precondition/4]).

-export([start/1, hungry/0, new_day/1, buy/2, stop/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         cheese_day/2, cheese_day/3,
         lettuce_day/2, lettuce_day/3,
         grapes_day/2,  grapes_day/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-type quantity() :: non_neg_integer().

-record(storage, { cheese = 5 :: quantity(),
                   lettuce = 5 :: quantity(),
                   grapes = 5 :: quantity() }).

-type food() :: 'cheese' | 'lettuce' | 'grapes'.
-type day() :: 'cheese_day' | 'lettuce_day' | 'grapes_day'.

-spec start(day()) ->
                   {'ok', pid()} | {'error', {'already_started', pid()}}.
start(Day) ->
    gen_fsm:start({local, creature}, ?MODULE, [Day], []).

stop() ->
    gen_fsm:send_all_state_event(creature, stop).

init([Day]) ->
    {ok, Day, #storage{}}.

-type food_left() :: 'cheese_left' | 'lettuce_left' | 'grapes_left'.

-spec hungry() -> {food_left(), quantity()}.
hungry() ->
    gen_fsm:sync_send_event(creature, eat).

-spec new_day(food()) -> 'ok'.
new_day(Food) ->
    gen_fsm:send_event(creature, {new_day, Food}).

-spec buy(food(), quantity()) -> 'ok'.
buy(Food, Quantity) ->
    gen_fsm:send_event(creature, {store, Food, Quantity}).

%% ----------------------------------------------------------------------
cheese_day(eat, Caller, #storage{cheese = Cheese} = S) ->
    gen_fsm:reply(Caller, {cheese_left, Cheese}),
    {next_state, cheese_day, S#storage{ cheese = Cheese - 1}}.

lettuce_day(eat, Caller, #storage{lettuce = Lettuce} = S) ->
    gen_fsm:reply(Caller, {lettuce_left, Lettuce}),
    {next_state, lettuce_day, S#storage{lettuce = Lettuce - 1}}.

grapes_day(eat, Caller, #storage{grapes = Grapes} = S) ->
    gen_fsm:reply(Caller, {grapes_left, Grapes}),
    {next_state, grapes_day, S#storage{grapes = Grapes - 1}}.


cheese_day({store, Food, Quantity}, S) ->
    case Food of
        cheese ->
            {next_state, cheese_day,
             S#storage{cheese = S#storage.cheese + Quantity}};
        lettuce ->
            {next_state, cheese_day,
             S#storage{lettuce = S#storage.lettuce + Quantity}};
        grapes ->
            {next_state, cheese_day,
             S#storage{grapes = S#storage.grapes + Quantity}}
    end;
cheese_day({new_day, lettuce}, S) ->
    {next_state, lettuce_day, S};
cheese_day({new_day, grapes}, S) ->
    {next_state, grapes_day, S}.

lettuce_day({store, Food, Quantity}, S) ->
    case Food of
        cheese ->
            {next_state, lettuce_day,
             S#storage{cheese = S#storage.cheese + Quantity}};
        lettuce ->
            {next_state, lettuce_day,
             S#storage{lettuce = S#storage.lettuce + Quantity}};
        grapes ->
            {next_state, lettuce_day,
             S#storage{grapes = S#storage.grapes + Quantity}}
    end;
lettuce_day({new_day, cheese}, S) ->
    {next_state, cheese_day, S};
lettuce_day({new_day, grapes}, S) ->
    {next_state, grapes_day, S}.

grapes_day({store, Food, Quantity}, S) ->
    case Food of
        cheese ->
            {next_state, grapes_day,
             S#storage{cheese = S#storage.cheese + Quantity}};
        lettuce ->
            {next_state, grapes_day,
             S#storage{lettuce = S#storage.lettuce + Quantity}};
        grapes ->
            {next_state, grapes_day,
             S#storage{grapes = S#storage.grapes + Quantity}}
    end;
grapes_day({new_day, cheese}, S) ->
    {next_state, cheese_day, S};
grapes_day({new_day, lettuce}, S) ->
    {next_state, lettuce_day, S}.

code_change(_, _, _, _) ->
    not_implemented.

terminate(_, _, _) ->
    not_implemented.

handle_info(_, _, _) ->
    not_implemented.

handle_sync_event(_, _, _, _) ->
    not_implemented.

handle_event(stop,_,S) ->
    {stop, normal, S}.



initial_state() ->
    cheese_day.

initial_state_data() ->
    #storage{}.

cheese_day(_S) ->
    store_transition() ++ eat_transition() ++
        [{grapes_day, {call, ?MODULE, new_day, [grapes]}},
         {lettuce_day, {call, ?MODULE, new_day, [lettuce]}}].

lettuce_day(_S) ->
    store_transition() ++ eat_transition() ++
        [{grapes_day, {call, ?MODULE, new_day, [grapes]}},
         {cheese_day, {call, ?MODULE, new_day, [cheese]}}].

grapes_day(_S) ->
    store_transition() ++ eat_transition() ++
        [{lettuce_day, {call,?MODULE,new_day,[lettuce]}},
         {cheese_day,  {call,?MODULE,new_day,[cheese]}}].

store_transition() ->
    [{history, {call, ?MODULE, buy, [food(), quantity()]}}].

eat_transition() ->
    [{history, {call, ?MODULE, hungry, []}}].

food() ->
    oneof([cheese, lettuce, grapes]).

quantity() ->
    ?SUCHTHATMAYBE(I, pos_integer(), I < 5).

precondition(Day, Day, _, {call, _, new_day, _}) ->
    false;
precondition(_, grapes_day, _, {call, _, new_day, [grapes]}) ->
    true;
precondition(_, cheese_day, _, {call,_,new_day,[cheese]}) ->
    true;
precondition(_, lettuce_day, _, {call,_,new_day,[lettuce]}) ->
    true;
precondition(_, _, _, {call,_,new_day,_}) ->
    false;
precondition(_, _, _, {call,_,_,_}) ->
    true.

postcondition(cheese_day, _, S, {call, _, hungry, []}, Result) ->
    Cheese = S#storage.cheese,
    Cheese > 0 andalso Result =:= {cheese_left, Cheese};
postcondition(lettuce_day, _, S, {call,_,hungry,[]}, Result) ->
    Lettuce = S#storage.lettuce,
    Lettuce > 0 andalso Result =:= {lettuce_left, Lettuce};
postcondition(grapes_day, _, S, {call,_,hungry,[]}, Result) ->
    Grapes = S#storage.grapes,
    Grapes > 0 andalso Result =:= {grapes_left, Grapes};
postcondition(_From, _Target, _StateData, {call,_,_,_}, Result) ->
    Result =:= ok.

next_state_data(_, _, S, _, {call,_,buy,[Food, Quantity]}) ->
    case Food of
        cheese ->
            S#storage{cheese = S#storage.cheese + Quantity};
        lettuce ->
            S#storage{lettuce = S#storage.lettuce + Quantity};
        grapes ->
            S#storage{grapes = S#storage.grapes + Quantity}
    end;
next_state_data(Today, _, S, _, {call,_,hungry,[]}) ->
    case Today of
        cheese_day ->
            S#storage{cheese = S#storage.cheese - 1};
        lettuce_day ->
            S#storage{lettuce = S#storage.lettuce - 1};
        grapes_day ->
            S#storage{grapes = S#storage.grapes - 1}
    end;
next_state_data(_From, _Target, StateData, _Result, {call,_,_,_}) ->
    StateData.

weight(_Today, _Tomorrow, {call,_,new_day,_}) -> 1;
weight(_Today, _Today, {call,_,hungry,_}) -> 3;
weight(_Today, _Today, {call,_,buy,_}) -> 2.

prop_doesnt_run_out_of_supplies() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
            begin
                start(cheese_day),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                stop(),
                ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                    [History, State, Result]),
                          aggregate(zip(proper_fsm:state_names(History),
                                        command_names(Cmds)),
                          Result =:= ok))
            end).

run() ->
    proper:quickcheck(prop_doesnt_run_out_of_supplies()).    

prop_test() ->
    ?assert(run).

