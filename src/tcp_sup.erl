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
-export([start/0, start/1, reload/1, start_link/1, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("fw.hrl").


start() ->
    start(tcp_srv).


start(Module) ->
    ChildSpec = {Module,
        {?MODULE, start_link, [Module]},
        permanent, infinity, supervisor, [?MODULE]
    },
    supervisor:start_child(fw_sup, ChildSpec),
    start_child(Module).


reload(Module) ->
    remove_server(Module),
    create_server(Module),
    start_child(Module).


create_server(Module) ->
    SocketParams = [binary, {active, false}, {reuseaddr, true}],
    ListenOnPort = fw_cfg:get_key(list_to_atom(lists:flatten(io_lib:format("~p_listen_port", [Module])))),
    {ok, Socket} = gen_tcp:listen(ListenOnPort, SocketParams),
    ?LOG_DEBUG("created a new ~p listening socket ~p on port ~p", [Module, Socket, ListenOnPort]),
    fw_srv:new_socket(Module, Socket).

remove_server(Module) ->
    Socket = fw_srv:get_socket(Module),
    gen_tcp:shutdown(Socket, read_write),
    gen_tcp:close(Socket),
    ?LOG_DEBUG("removed the ~p listening socket ~p", [Module, Socket]).


%% @spec start_link(Module) -> {ok, pid()}
%% where
%%    Module = atom()
%% @doc Called by the root supervisor and starts the TCP supervisor process.
%%    Calls {@module}:init/1 in the spawned process.
start_link(Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Module]).


%% @spec start_child() -> {ok, pid()}
%% @doc Called by TCP worker handlers to start new handlers.
start_child(Module) ->
    supervisor:start_child(?MODULE, [fw_srv:get_socket(Module)]).


%% @spec init(Args) -> {ok, supervisor_result()}
%% where
%%    Args = [Module]
%%    Module = atom()
%% @doc Supervisor behaviour init callback.  Spawns worker processes
%%    to handle incoming TCP connections.
init([Module] = _Args) ->
    create_server(Module),
    Server = {tcp_srv, {tcp_srv, start_link, [fw_cfg:get_key(socket_read_timeout), Module]},
              temporary, brutal_kill, worker, [tcp_srv]},
    {ok, {{simple_one_for_one, 0, 1}, [Server]}}.
