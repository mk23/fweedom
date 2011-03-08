%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_server.html" target="_blank">OTP gen_server</a>
%%   documentation for more information.
%%
%% @doc telephone-server - Configuration utility library.
%%   This module provides functions to handle global application configuration.

-module(ts_cfg).

%% API
-export([start/0]).
-export([
    get_key/1,
    get_key/2,
    set_key/2,
    add_key/2,
    reload/0
]).

-include("ts.hrl").

%% API implementation
start() ->
    ets:new(?MODULE, [named_table, public]),
    reload().

get_key(Key) ->
    get_key(Key, undefined).

get_key(Key, Def) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Val}] ->
            Val;
        [] ->
            Def
    end.

set_key(Key, Val) ->
    ets:insert(?MODULE, {Key, Val}).

add_key(Key, Val) ->
    case get_key(Key, []) of
        List when is_list(List) ->
            case lists:member(Val, List) of
                false ->
                    set_key(Key, [Val|List]);
                true ->
                    ?LOG_DEBUG("add_mod() found duplicate entry in list for key ~p: ~p", [Key, Val])
            end;
        Item ->
            set_key(Key, [Val, Item])
    end.

reload() ->
    try
        {ok, Els} = file:consult(os:getenv("TS_CONFIG_FILE")),
        {ts, Cfg} = lists:keyfind(ts, 1, Els),
        Fun = fun
            ({Key, Def}) ->
                case lists:keyfind(Key, 1, Cfg) of
                    {Key, Val} ->
                        set_key(Key, Val);
                    _ ->
                        set_key(Key, Def)
                end;
            (Key) ->
                case lists:keyfind(Key, 1, Cfg) of
                    {Key, Val} ->
                        set_key(Key, Val);
                    _ ->
                        ok
                end
        end,
        lists:foreach(Fun, ?CONFIG_KEYS)
    catch
        _:_ ->
            ok
    end.
