%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_server.html" target="_blank">OTP gen_server</a>
%%   documentation for more information.
%%
%% @doc telephone-server - Dynamic configuration server.
%%   This module provides all necessary callbacks to create a server that handles application
%%   configuration.

-module(ts_cfg).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0]).
-export([
    get_key/1,
    get_key/2,
    set_key/2,
    get_mod/1,
    get_mod/2,
    set_mod/2,
    reload/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-include("ts.hrl").

-record(state, {conf = dict:new(), mods = dict:new()}).

%% API implementation
start() ->
    ChildSpec = {?MODULE,
        {?MODULE, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [?MODULE]
    },
    supervisor:start_child(ts_sup, ChildSpec).

%% @spec start_link() -> {ok, pid()}
%% @doc Called by the root supervisor start.  Creates a server process
%%    that handles application configuration. Calls {@module}:init/1 in
%%    the spawned process.
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_key(Key) ->
    get_key(Key, undefined).

get_key(Key, Def) ->
    gen_server:call({global, ?MODULE}, {get_key, Key, Def}).

set_key(Key, Val) ->
    gen_server:cast({global, ?MODULE}, {set_key, Key, Val}).

get_mod(Key) ->
    get_mod(Key, undefined).

get_mod(Key, Def) ->
    gen_server:call({global, ?MODULE}, {get_mod, Key, Def}).

set_mod(Key, Val) ->
    gen_server:cast({global, ?MODULE}, {set_mod, Key, Val}).

reload() ->
    gen_server:cast({global, ?MODULE}, reload).


%% Internal functions
read_config() ->
    try
        {ok, Els} = file:consult(os:getenv("TS_CONFIG_FILE")),
        {ts, Cfg} = lists:keyfind(ts, 1, Els),
        Fun = fun
            ({Key, Def}, Acc) ->
                case lists:keyfind(Key, 1, Cfg) of
                    {Key, Val} ->
                        dict:store(Key, Val, Acc);
                    _ ->
                        dict:store(Key, Def, Acc)
                end;
            (Key, Acc) ->
                case lists:keyfind(Key, 1, Cfg) of
                    {Key, Val} ->
                        dict:store(Key, Val, Acc);
                    _ ->
                        Acc
                end
        end,
        lists:foldl(Fun, dict:new(), ?CONFIG_KEYS)
    catch
        _:_ ->
            dict:new()
    end.


%% gen_server callbacks

%% @spec init(Args) -> {ok, supervisor_result()}
%% where
%%    Args = []
%% @doc gen_server behaviour init callback.  Starts the configuration
%%    server.
init([]) ->
    {ok, #state{conf = read_config()}}.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({set_key, Key, Value}, State) ->
    {noreply, State#state{conf = dict:store(Key, Value, State#state.conf)}};

handle_cast({set_mod, Key, Value}, State) ->
    {noreply, State#state{mods = dict:store(Key, Value, State#state.mods)}};

handle_cast(reload, State) ->
    {noreply, State#state{conf = read_config()}};

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_call({get_key, Key, Default}, _From, State) ->
    case dict:find(Key, State#state.conf) of
        {ok, Value} ->
            {reply, Value, State};
        error ->
            {reply, Default, State}
    end;

handle_call({get_mod, Key, Default}, _From, State) ->
    case dict:find(Key, State#state.mods) of
        {ok, Value} ->
            {reply, Value, State};
        error ->
            {reply, Default, State}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    {ok, Reason}.
