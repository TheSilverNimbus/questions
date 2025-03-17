%%%-------------------------------------------------------------------
%% @doc questions public API
%% @end
%%%-------------------------------------------------------------------

-module(questions_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    questions_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
