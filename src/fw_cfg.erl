%% @author Max Kalika <max.kalika+framework@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/ets.html" target="_blank">Erlang ETS</a>
%%   documentation for more information.
%%
%% @doc Application Framework - Configuration utility library.
%%   This module provides functions to handle global system configuration.

-module(fw_cfg).

%% API
-export([
    get_key/1,
    get_key/2,
    set_key/2,
    add_key/2,
    reload/0
]).

-include("fw.hrl").

%% API
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
    ?LOG_DEBUG("setting config key ~p: ~9999p", [Key, Val]),
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
    Fun = fun({K, V}) -> set_key(K, V) end,
    try
        {ok, Cfg} = file:consult(os:getenv("FW_CONFIG_FILE")),
        lists:foreach(Fun, lists:ukeymerge(1, lists:ukeysort(1, Cfg), lists:ukeysort(1, ?CONFIG_KEYS)))
    catch
        _:_ ->
            ?LOG_ERROR("failed to load config file, starting with defaults", []),
            lists:foreach(Fun, ?CONFIG_KEYS)
    end.
