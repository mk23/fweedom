%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/supervisor.html" target="_blank">OTP supervisor</a>
%%   documentation for more information.
%%
%% @doc telephone-server - User registration handler and supervisor controller.
%%   This module provides all necessary callbacks to handle user registration requests and manage
%%   in-progress registrations workers.
%%
%% @type supervisor_result() = term().  Please see OTP
%%    <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1" target="_blank">documentation</a>
%%    for more information.

-module(uh_user).

%% API
-export([start/0, start_link/0]).

%% Web server callbacks
-export([handle_get/2, handle_post/2]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").
-include("web_server.hrl").

start() ->
    web_server:uri_register("user", ?MODULE, ['GET', 'POST']),
    ChildSpec = {?MODULE,
        {?MODULE, start_link, []},
        permanent,
        infinity,
        supervisor,
        [?MODULE]
    },
    supervisor:start_child(ts_sup, ChildSpec).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init(Args) -> {ok, supervisor_result()}
%% where
%%    Args = []
%% @doc Supervisor behaviour init callback.  Spawns worker processes
%%    to handle maintain in-progress registration requests.
init([] = _Args) ->
    Server = {user_server, {user_server, start_link, []},
              temporary, brutal_kill, worker, [user_server]},
    {ok, {{simple_one_for_one, 0, 1}, [Server]}}.

handle_get(["aprove", Token], Req) ->
    ?LOG_DEBUG("handling user confirmation request: ~p", [Req]),
    {Login, Pid} = binary_to_term(base64:decode(Token)),
    case rpc:pinfo(Pid, status) of
        {status, _} ->
            ?LOG_DEBUG("found existing registration process: ~p for ~p", [Pid, Login]),
%            gen_server:cast(Pid, {confirm, Login}).
            {201, <<"created\n">>};
        _ ->
            ?LOG_INFO("no registration process found for pid: ~p", [Pid]),
            {404, <<"invalid token\n">>}
    end.

handle_post(["create", Login], Req) ->
    ?LOG_DEBUG("handling user registration request: ~p", [Req]),
    {ok, Pid} = supervisor:start_child(?MODULE, [Login, web_server:parse_qstring(Req#req.body)]),
    Token = base64:encode(term_to_binary({Login, Pid})),
    {200, <<"Registration started: ", Token/bytes, $\n>>}.

