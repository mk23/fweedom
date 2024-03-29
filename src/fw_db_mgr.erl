-module(fw_db_mgr).

-export([start/0, update_table/2]).

-include("fw.hrl").


start() ->
    update_table(?MODULE, fw_tbl_vsn).


?DB_FINISH_TBL(fw_tbl_vsn, 1);
update_table(fw_tbl_vsn = T, 0 = V) ->
    Params = [
        {type, set},
        {disc_only_copies, [node()|nodes()]},
        {attributes, record_info(fields, fw_tbl_vsn)}
    ],
    ?DB_ATOMIC_ACT(create_table, [T, Params]),
    ?DB_UPDATE_TBL(T, V);

update_table(M, T) ->
    case catch mnesia:table_info(T, wild_pattern) of
        Result when is_tuple(Result) andalso T =:= element(1, Result) ->
            [{fw_tbl_vsn, T, V}] = mnesia:dirty_match_object({fw_tbl_vsn, T, '_'}),
            ?LOG_INFO("upgrading table: ~p to ~p", [T, V]),
            M:update_table(T, V);
        {'EXIT', {aborted, {no_exists, T, wild_pattern}}} ->
            ?LOG_WARN("doesn't exist, creating: ~p", [T]),
            M:update_table(T, 0)
    end.
