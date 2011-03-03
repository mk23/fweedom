%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/supervisor.html" target="_blank">OTP supervisor</a>
%%   documentation for more information.
%%
%% @doc telephone-server - Top-level supervisor controller.
%%   This module provides the necessary callbacks to create the root of the supervisor tree.
%%
%% @type supervisor_result() = term().  Please see OTP
%%    <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1" target="_blank">documentation</a>
%%    for more information.

-module(ts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").

%% @spec start_link() -> {ok, pid()}
%% @doc Called by the application start.  Creates the top-level supervisor
%%    process. Calls {@module}:init/1 in the spawned process.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init(Args) -> {ok, supervisor_result()}
%% where
%%    Args = []
%% @doc Supervisor behaviour init callback. Starts the TCP listener supervisor,
%%    the store server worker, and the stats server worker.
init([] = _Args) ->
    TCPSup = {tcp_sup, {tcp_sup, start_link, []},
              permanent, 2000, supervisor, [tcp_sup]},
    {ok, {{one_for_one, 0, 1}, [TCPSup]}}.
