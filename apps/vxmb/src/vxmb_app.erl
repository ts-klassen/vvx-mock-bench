%%%-------------------------------------------------------------------
%% @doc vxmb public API
%% @end
%%%-------------------------------------------------------------------

-module(vxmb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    vxmb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
