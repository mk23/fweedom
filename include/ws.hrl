-define(END_REQUEST(S), {stop, normal, ok}).

%% @type uri() = { scheme = http | https | ftp | binary()
%%                 host = string()
%%                 port = int() | undefined
%%                 path = [string()]
%%               }
-record(uri, {scheme = http, host, port = fw_cfg:get_key(listen_port), path}).

%% @type req() = { s = Socket
%%                 method = 'GET' | 'POST' | 'PUT' | 'DELETE'
%%                 module = atom()
%%                 uri = uri()
%%                 qry = [{Key, Val}]
%%                 vsn = {1,0} | {1,1}
%%                 head = [{HeadKey, HeadVal}]
%%                 body = binary()
%%                 left = int()
%%               }
%% where
%%    Socket = port()
%%    Key = Val = string()
%%    HeadKey = atom() | binary()
%%    HeadVal = atom() | int() | binary()
-record(req, {s, method, module, uri = #uri{}, qry = [], vsn = {1, 0}, head = [], body = <<>>, left = 0}).
