%%%-------------------------------------------------------------------
%% @doc erlang_functional_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_functional_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_functional_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
