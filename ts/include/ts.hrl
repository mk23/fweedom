-define(CONFIG_KEYS, [
    {reg_timeout, 3600}
]).

-record(user_data, {login, email, alias, secret, created, changed}).
