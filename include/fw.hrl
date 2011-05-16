-define(LOG_MODULE, fw_log_mod).
-define(LOG_CRIT(Format,  Params), ?LOG_MODULE:log_crit(?MODULE,  ?LINE, Format, Params)).
-define(LOG_ERROR(Format, Params), ?LOG_MODULE:log_error(?MODULE, ?LINE, Format, Params)).
-define(LOG_WARN(Format,  Params), ?LOG_MODULE:log_warn(?MODULE,  ?LINE, Format, Params)).
-define(LOG_INFO(Format,  Params), ?LOG_MODULE:log_info(?MODULE,  ?LINE, Format, Params)).
-define(LOG_DEBUG(Format, Params), ?LOG_MODULE:log_debug(?MODULE, ?LINE, Format, Params)).

-define(FW_CONFIG_KEYS, [
    {log_level, error},
    {http_handlers, []},
    {tcp_srv_listen_port,  1123},
    {http_srv_listen_port, 1123},
    {socket_read_timeout, infinity}
]).

-define(DB_ATOMIC_TXN(F),
    ?LOG_DEBUG("db transaction: ~p", [F]),
    {__Res, __Val} = mnesia:transaction(F),
    ?LOG_DEBUG("db transaction: ~p: result: ~p: ~9999p", [F, __Res, __Val])
).

-define(DB_ATOMIC_ACT(F, A),
    ?LOG_DEBUG("db action: mnesia:~p() params: ~9999p", [F, A]),
    {__Res, __Val} = apply(mnesia, F, A),
    ?LOG_DEBUG("db action: mnesia:~p() params: ~9999p: result: ~p: ~9999p", [F, A, __Res, __Val])
).

-define(DB_UPDATE_TBL(T, V),
    mnesia:dirty_write({fw_tbl_vsn, T, V + 1}),
    ?MODULE:update_table(T, V + 1)
).

-define(DB_FINISH_TBL(T, V),
    update_table(T, V) ->
        ?LOG_INFO("table upgraded: ~p (~p)", [T, V])
).

-record(fw_tbl_vsn, {name, version}).
