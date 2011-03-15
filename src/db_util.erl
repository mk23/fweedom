-module(db_util).

-export([upgrade_table/1]).

-include("ts.hrl").

-define(BUMP_VERSION(T, V), mnesia:dirty_write({table_vsn, T, V+1}), upgrade_table(T, V+1)).

upgrade_table(T) ->
    case catch mnesia:table_info(T, wild_pattern) of
        Result when is_tuple(Result) andalso T =:= element(1, Result) ->
            [{table_vsn, T, V}] = mnesia:dirty_match_object({table_vsn, T, '_'}),
            ?LOG_INFO("upgrade_table() upgrading table: ~p to ~p", [T, V]),
            upgrade_table(T, V);
        {'EXIT', {aborted, {no_exists, T, wild_pattern}}} ->
            ?LOG_WARN("upgrade_table() doesn't exist, creating: ~p", [T]),
            upgrade_table(T, 0)
    end.


%% user_data
upgrade_table(user_data, 1) ->
    ok;
upgrade_table(user_data = T, 0 = V) ->
    Params = [
        {type, set},
        {disc_copies, [node()|nodes()]},
        {attributes, record_info(fields, user_data)}
    ],
    {atomic, ok} = mnesia:create_table(T, Params),
    ?BUMP_VERSION(T, V);


%% table_vsn
upgrade_table(table_vsn, 1) ->
    ok;
upgrade_table(table_vsn = T, 0 = V) ->
    Params = [
        {type, set},
        {disc_only_copies, [node()|nodes()]},
        {attributes, record_info(fields, table_vsn)}
    ],
    {atomic, ok} = mnesia:create_table(T, Params),
    ?BUMP_VERSION(T, V).
