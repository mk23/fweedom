-define(END_REQUEST(S), {stop, normal, ok}).

-record(uri, {scheme = http, host, port = ts_cfg:get_key(listen_port), path}).
-record(req, {s, method, module, uri = #uri{}, qry = [], vsn = {1, 0}, head = [], body = <<>>, left = 0}).
