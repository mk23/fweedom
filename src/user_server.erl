-module(user_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

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

-record(state, {login, data}).

start_link(Login, Data) ->
    ?LOG_INFO("starting user registration session: ~p", [Login]),
    gen_server:start_link(?MODULE, [Login, Data], []).

init([Login, Data]) ->
    ?LOG_DEBUG("created registration grace period: ~p: ~p", [Login, Data]),
    {ok, #state{login = Login, data = Data}, ts_cfg:get_key(reg_timeout) * 1000}.

handle_info(timeout, State) ->
    ?LOG_INFO("registration grace period expired: ~p", [State#state.login]),
    {stop, normal, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {ok, Reason}.
