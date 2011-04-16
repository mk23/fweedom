-module(user_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

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

-record(state, {login, type, data}).

start_link(Type, Login, Data) ->
    ?LOG_INFO("starting user request (~p) session: ~p", [Type, Login]),
    gen_server:start_link(?MODULE, [Login, Data], []).

init([Type, Login, Data]) ->
    ?LOG_DEBUG("created request (~p) grace period: ~p: ~p", [Type, Login, Data]),
    {ok, #state{login = Login, type = Type, data = Data}, fw_cfg:get_key(reg_timeout) * 1000}.

handle_info(timeout, State) ->
    ?LOG_INFO("user request (~p) grace period expired: ~p", [State#state.type, State#state.login]),
    {stop, normal, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call({verify, Login}, _From, #state{type = create, login = Login} = State) ->
    {reply, ok, State};

handle_call({verify, Login}, _From, #state{type = change, login = Login} = State) ->
    {reply, ok, State};

handle_call({verify, Login}, _From, State) ->
    ?LOG_ERROR("invalid confirmation request (~p) for ~p: expected ~p", [Login, State#state.type, State#state.login]),
    {reply, false, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {ok, Reason}.
