%% @author Max Kalika <max.kalika+framework@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_server.html" target="_blank">OTP gen_server</a> and
%%   <a href="http://www.erlang.org/doc/man/gen_tcp.html" target="_blank">Erlang TCP</a>
%%   documentation for more information.
%%
%% @doc Application Framework - TCP worker
%%   This module provides all necessary callbacks to create a TCP connection hanlding server.
%%   The initializer returns a timeout of 0 to the gen_server controller which immediatly sends
%%   a timeout message to the handler, causing it to wait for a new connection.

%% TODO: function edocs
%% TODO: function eunits

-module(tcp_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, start_link/3]).

%% tcp_server callbacks
-export([handle_data/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-include("fw.hrl").

-record(state, {socket, module, client = new, accept = true, timeout = infinity}).

start_link(Socket) ->
    start_link(Socket, ?MODULE, infinity).

start_link(Socket, Module) when is_atom(Module) ->
    start_link(Socket, Module, infinity);

start_link(Socket, Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    start_link(Socket, ?MODULE, Timeout).

start_link(Socket, Module, Timeout) ->
    ?LOG_DEBUG("starting tcp server for: ~p", [Module]),
    gen_server:start_link(?MODULE, [Socket, Module, Timeout], []).


init([Socket, Module, Timeout]) ->
    {ok, #state{socket = Socket, module = Module, timeout = Timeout}, 0}.


handle_info({tcp, Socket, Packet}, #state{module = Module, timeout = Timeout} = State) ->
    ?LOG_DEBUG("received data from client: ~p: ~9999p", [Socket, Packet]),
    Client = Module:handle_data(Socket, Packet, State#state.client),
    ?LOG_DEBUG("new client state from handler: ~9999p", [Client]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{client = Client}, Timeout};

handle_info({tcp_closed, Socket}, State) ->
    ?LOG_INFO("client disconnected: ~p", [Socket]),
    {stop, normal, State};

handle_info(timeout, #state{accept = true, timeout = Timeout} = State) ->
    {ok, Socket} = gen_tcp:accept(State#state.socket),
    ?LOG_INFO("new client connected: ~p", [Socket]),
    tcp_sup:start_child(),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket, accept = false}, Timeout};

handle_info(timeout, #state{accept = false} = State) ->
    ?LOG_INFO("socket read timeout: ~p", [State#state.socket]),
    {stop, normal, State}.


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_call(_Req, _From, State) ->
    {reply, ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    {ok, Reason}.


handle_data(Socket, Packet, _Client) ->
    gen_tcp:send(Socket, Packet).
