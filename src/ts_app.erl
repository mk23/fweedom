%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://erlang.org/doc/design_principles/applications.html" target="_blank">OTP applications</a>
%%   documentation for more information.
%%
%% @doc telephone-server - Top-level application controller
%%   This module provides the necessary callbacks to start the telephone server
%%   application. It uses the ts.app resource file to determine the function and
%%   arguments.

-module(ts_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

-include("ts.hrl").

%% @spec start(Type, Args) -> {ok, pid()}
%% where
%%    Type = normal
%%    Args = term()
%% @doc Application behaviour start callback.  Starts the top-level supervisor.
start(normal = _Type, _Args) ->
    ts_sup:start_link().

%% @spec stop(State) -> ok
%% where
%%    State = term()
%% @doc Application behaviour stop callback.  No-op.
stop(_State) ->
    ok.
