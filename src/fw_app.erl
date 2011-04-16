%% @author Max Kalika <max.kalika+framework@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://erlang.org/doc/design_principles/applications.html" target="_blank">OTP applications</a>
%%   documentation for more information.
%%
%% @doc Application Framework - Top-level application controller.
%%   This module provides the necessary callbacks to start the framework server
%%   application. It uses the fw.app resource file to determine the function and
%%   arguments.

-module(fw_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

-include("fw.hrl").

%% @spec start(Type, Args) -> {ok, pid()}
%% where
%%    Type = normal
%%    Args = term()
%% @doc Application behaviour start callback.  Starts the root supervisor.
start(normal = _Type, _Args) ->
    {ok, Pid} = fw_sup:start_link(),
    fw_cfg:start(),
    fw_log:start(),

    db_mgr:update_table(table_vsn),
    web_srv:start(),
    {ok, Pid}.

%% @spec stop(State) -> ok
%% where
%%    State = term()
%% @doc Application behaviour stop callback.  No-op.
stop(_State) ->
    ok.
