%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc Control a trade_fsm from the outside
%%%   This module implements a trade_fsm controller which can steer a
%%%   trade_fsm. The most prominent feature is that it converts
%%%   blocking calls into future promises.
%%% @end
%%% Created : 31 Oct 2011 by Jesper Louis andersen <>
%%%-------------------------------------------------------------------
-module(trade_fsm_controller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         unblock/0, unblock/1,
         trade/1, accept_trade/0, make_offer/1,
         retract_offer/1, ready/0, cancel/0,
         ask_negotiate/2,

         accept_negotiate/1, do_offer/1, undo_offer/1,
         are_you_ready/0, not_yet/0, am_ready/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TRADE_FSM, trade_fsm).

-record(state, {
          fsm = undefined :: undefined | pid(),
          key = undefined :: undefined | pid() | {reply, term()} }).

%%%===================================================================

%% @doc Starts the server
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Instruct the trade_fsm to trade
%% @end
trade(Trader) ->
    call({trade, Trader}).

%% @doc Accept the trade connection
%% @end
accept_trade() ->
    call(accept_trade).

%% @doc Make an offer
%% @end
make_offer(Item) ->
    cast({propagate, make_offer, Item}).

%% @doc Retract an offer
%% @end
retract_offer(Item) ->
    cast({propagate, retract_offer, Item}).

%% @doc Tell the other end we are ready
%% @end
ready() ->
    call(ready).

%% @doc Tell the other end to call
%% @end
cancel() ->
    call(cancel).

unblock() ->
    unblock(5000).

unblock(Timeout) ->
    call({unblock, Timeout}).

stop() ->
    call(stop).

%% API
ask_negotiate(Me, Mod) ->
    cast({ask_negotiate, Me, Mod}).

accept_negotiate(Me) ->
    cast({accept_negotiate, Me}).

do_offer(Item) ->
    cast({cast, {do_offer, Item}}).

undo_offer(Item) ->
    cast({cast, {undo_offer, Item}}).

are_you_ready() ->
    cast({cast, are_you_ready}).

not_yet() ->
    cast({cast, not_yet}).

am_ready() ->
    cast({cast, not_yet}).

%%%===================================================================

%% @private
init([]) ->
    {ok, Pid} = trade_fsm:start_link("A"),
    {ok, #state{ fsm = Pid }}.

%% @private
handle_call({unblock, _Timeout}, _From, #state { key = {reply, R} } = State) ->
    {reply, R, State#state { key = undefined }};
handle_call({unblock, Timeout}, _From, #state { key = K } = State) when is_pid(K) ->
    Reply = rpc:nb_yield(K, Timeout),
    io:format("unblock on ~p with value ~p\n", [K, Reply]),
    {reply, Reply, State#state { key = case Reply of
                                           timeout -> K;
                                           {value, _} -> undefined
                                       end }};
handle_call(accept_trade, _From, State) ->
    Reply = direct_call(accept_trade, [], State),
    {reply, Reply, State};
handle_call(cancel, _From, #state { key = undefined } = State) ->
    Key = async_call(cancel, [], State),
    {reply, ok, State#state { key = Key } };
handle_call(ready, _From, #state { key = undefined } = State) ->
    Key = async_call(ready, [], State),
    {reply, ok, State#state { key = Key } };
handle_call({trade, Trader}, _From, #state { key = undefined } = State) ->
    Key = async_call(trade, [Trader], State),
    {reply, ok, State#state { key = Key } };
handle_call(stop, From, State) ->
    Reply = direct_call(cancel, [], State),
    gen_fsm:reply(From, Reply),
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({propagate, Cmd, Item}, State) ->
    direct_call(Cmd, [Item], State),
    {noreply, State};
handle_cast({ask_negotiate, Me, Mod}, #state { fsm = Pid } = State) ->
    trade_fsm:ask_negotiate(Pid, Me, Mod),
    {noreply, State};
handle_cast({accept_negotiate, Me}, #state { fsm = Pid} = State) ->
    trade_fsm:accept_negotiate(Pid, Me),
    {noreply, State};
handle_cast({cast, What}, #state { fsm = Pid } = State) ->
    case What of
        {do_offer, Item} ->
            trade_fsm:do_offer(Pid, Item);
        {undo_offer, Item} ->
            trade_fsm:undo_offer(Pid, Item);
        are_you_ready ->
            trade_fsm:are_you_ready(Pid);
        not_yet ->
            trade_fsm:not_yet(Pid);
        am_ready ->
            trade_fsm:am_ready(Pid)
    end,
    {noreply, State};
handle_cast(stop, State) ->
    trade_fsm:stop(),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({Key, {promise_reply, R}}, #state { key = Key } = S) ->
    {noreply, S#state { key = {reply, R} }};
handle_info(Info, State) ->
    io:format("WARNING, UNKOWN HANDLE_INFO: ~p\n", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
async_call(F, Args, #state { fsm = Pid })
  when is_pid(Pid); is_atom(Pid) ->
    K = rpc:async_call(node(), ?TRADE_FSM, F, [Pid | Args]),
    io:format("Async Call: ~p key = ~p\n", [F, K]),
    K.

direct_call(F, Args, #state { fsm = Pid })
  when is_pid(Pid) ->
    apply(?TRADE_FSM, F, [Pid | Args]).

call(M) ->
    gen_server:call(?MODULE, M, infinity).

cast(M) ->
    gen_server:cast(?MODULE, M).

