%% @author Max Kalika <max.kalika+telenphone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_server.html" target="_blank">OTP gen_server</a> and
%%   <a href="http://www.erlang.org/doc/man/gen_tcp.html" target="_blank">Erlang TCP</a>
%%   documentation for more information.
%%
%% @doc telephone-server - TCP worker
%%   This module provides all necessary callbacks to create a TCP connection hanlding server.
%%   The initializer returns a timeout of 0 to the gen_server controller which immediatly sends
%%   a timeout message to the handler, causing it to wait for a new connection.

%% TODO: function edocs
%% TODO: function eunits

-module(tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

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

-record(state, {socket, module, client}).

start_link({Socket, Module}) ->
    gen_server:start_link(?MODULE, [Socket, Module], []);

start_link(Socket) ->
    start_link({Socket, ?MODULE}).


init([Socket, Module]) ->
    {ok, #state{socket = Socket, module = Module, client = {}}, 0}.

handle_info({tcp, Socket, Packet}, State) ->
	Module = State#state.module,
	Client = Module:handle_data(Socket, Packet, State#state.client),
	inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{client = Client}};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.socket),
	inet:setopts(Socket, [{active, once}]),
    tcp_sup:start_child(),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
	{noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

code_change(_, _, State) ->
    {ok, State}.

terminate(Reason, _State) ->
    {ok, Reason}.


handle_data(Socket, Packet, _Client) ->
    gen_tcp:send(Socket, Packet).
