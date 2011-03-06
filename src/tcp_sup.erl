%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/supervisor.html" target="_blank">OTP supervisor</a>
%%   documentation for more information.
%%
%% @doc telephone-server - TCP supervisor controller.
%%   This module provides all necessary callbacks to create the supervisor for TCP workers.
%%
%% @type supervisor_result() = term().  Please see OTP
%%    <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1" target="_blank">documentation</a>
%%    for more information.

-module(tcp_sup).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").

start() ->
    ChildSpec = {?MODULE,
        {?MODULE, start_link, []},
        permanent,
        infinity,
        supervisor,
        [?MODULE]
    },
    supervisor:start_child(ts_sup, ChildSpec).

%% @spec start_link() -> {ok, pid()}
%% @doc Called by the root supervisor and starts the TCP supervisor process.
%%    Calls {@module}:init/1 in the spawned process.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec start_child() -> {ok, pid()}
%% @doc Called by TCP worker handlers to start new handlers.
start_child() ->
    supervisor:start_child(?MODULE, []).

%% @spec init(Args) -> {ok, supervisor_result()}
%% where
%%    Args = []
%% @doc Supervisor behaviour init callback.  Spawns worker processes
%%    to handle incoming TCP connections.
init([] = _Args) ->
	SocketParams = [binary, {active, false}, {reuseaddr, true}],
    {ok, Socket} = gen_tcp:listen(ts_cfg:get_key(listen_port), SocketParams),
	
    Server = {tcp_server, {tcp_server, start_link, [Socket]},
              temporary, brutal_kill, worker, [tcp_server]},
    {ok, {{simple_one_for_one, 0, 1}, [Server]}}.
