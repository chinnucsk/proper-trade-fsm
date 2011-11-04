-module(qc).

-export([qc/0]).

qc() ->
    trade_fsm_proper:qc().
