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

-define(FW_UPDATE_TABLE(T, V), mnesia:dirty_write({fw_tbl_vsn, T, V+1}), ?MODULE:update_table(T, V+1)).
-record(fw_tbl_vsn, {name, version}).
