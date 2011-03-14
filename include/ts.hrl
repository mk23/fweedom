-define(LOG_MODULE, ts_log_mod).
-define(LOG_CRIT(Format,  Params), ?LOG_MODULE:log_crit(?MODULE,  ?LINE, Format, Params)).
-define(LOG_ERROR(Format, Params), ?LOG_MODULE:log_error(?MODULE, ?LINE, Format, Params)).
-define(LOG_WARN(Format,  Params), ?LOG_MODULE:log_warn(?MODULE,  ?LINE, Format, Params)).
-define(LOG_INFO(Format,  Params), ?LOG_MODULE:log_info(?MODULE,  ?LINE, Format, Params)).
-define(LOG_DEBUG(Format, Params), ?LOG_MODULE:log_debug(?MODULE, ?LINE, Format, Params)).

-define(CONFIG_KEYS, [
    {log_file, ""},
    {log_level, error},
    {listen_port, 1123},
    {reg_timeout, 3600}
]).
