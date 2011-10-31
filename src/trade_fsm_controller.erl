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
-export([start_link/0, trade/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

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
    gen_server:cast(?MODULE, {trade, Trader}).

%%%===================================================================

%% @private
init([]) ->
    {ok, Pid} = trade_fsm:start_link("A"),
    {ok, #state{ fsm = Pid }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({trade, Trader}, State) ->
    Key = async_call(trade, [Trader], State),
    {noreply, State#state { key = Key }};
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
async_call(F, Args, #state { fsm = Pid }) when is_pid(Pid) ->
    rpc:async_call(node(), trade_fsm, F, [Pid | Args]).

