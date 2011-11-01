%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc Control a trade_fsm from the outside
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
         retract_offer/1, ready/0, cancel/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TRADE_FSM, trade_fsm).

-record(state, {
          fsm = undefined :: undefined | pid(),
          key = undefined :: undefined | pid() }).

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
    cast(stop).

%%%===================================================================

%% @private
init([]) ->
    {ok, Pid} = trade_fsm:start_link("A"),
    {ok, #state{ fsm = Pid }}.

%% @private
handle_call({unblock, Timeout}, _From, #state { key = K } = State) ->
    Reply = rpc:nb_yield(K, Timeout),
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({propagate, Cmd, Item}, State) ->
    direct_call(Cmd, [Item], State),
    {noreply, State};
handle_cast(stop, State) ->
    trade_fsm:stop(),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
async_call(F, Args, #state { fsm = Pid })
  when is_pid(Pid) ->
    rpc:async_call(node(), ?TRADE_FSM, F, [Pid | Args]).

direct_call(F, Args, #state { fsm = Pid })
  when is_pid(Pid) ->
    apply(?TRADE_FSM, F, [Pid | Args]).

call(M) ->
    gen_server:call(?MODULE, M, infinity).

cast(M) ->
    gen_server:call(?MODULE, M).

