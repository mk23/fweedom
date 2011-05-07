%%TODO: split this into framework and other app

-module(db_mgr).

-export([update_table/1]).

-include("fw.hrl").

-define(BUMP_VERSION(T, V), mnesia:dirty_write({table_vsn, T, V+1}), update_table(T, V+1)).

update_table(T) ->
    case catch mnesia:table_info(T, wild_pattern) of
        Result when is_tuple(Result) andalso T =:= element(1, Result) ->
            [{table_vsn, T, V}] = mnesia:dirty_match_object({table_vsn, T, '_'}),
            ?LOG_INFO("upgrading table: ~p to ~p", [T, V]),
            update_table(T, V);
        {'EXIT', {aborted, {no_exists, [T, _]}}} ->
            ?LOG_WARN("doesn't exist, creating: ~p", [T]),
            update_table(T, 0)
    end.


%% user_data
%update_table(user_data, 1) ->
%    ok;
%update_table(user_data = T, 0 = V) ->
%    Params = [
%        {type, set},
%        {disc_copies, [node()|nodes()]},
%        {attributes, record_info(fields, user_data)}
%    ],
%    {atomic, ok} = mnesia:create_table(T, Params),
%    ?BUMP_VERSION(T, V);


%% table_vsn
update_table(table_vsn = T, 1 = V) ->
    ?LOG_INFO("db upgrade complete: ~p v~p", [T, V]);
update_table(table_vsn = T, 0 = V) ->
    Params = [
        {type, set},
        {disc_only_copies, [node()|nodes()]},
        {attributes, record_info(fields, table_vsn)}
    ],
    {atomic, ok} = mnesia:create_table(T, Params),
    ?BUMP_VERSION(T, V).
