%% @author Max Kalika <max.kalika+framework@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_tcp.html" target="_blank">Erlang TCP</a>
%%   documentation for more information.
%%
%% @doc Application Framework - HTTP requests handler
%%   This module provides functions to handle a subset of HTTP/1.1 requests and dispatches
%%   them to registered uri handlers implemented by other modules.

-module(http_srv).

-behaviour(tcp_srv).

%% API
-export([start/0]).
-export([parse_qstring/1]).
-export([send_packet/2, send_packet/3, send_packet/4]).

%% http_srv interface
-export([uri_register/2, uri_register/3]).

%% tcp_srv callback
-export([handle_init/0, handle_data/3]).

-include("fw.hrl").
-include("http_srv.hrl").


start() ->
    tcp_sup:start(?MODULE),
    load_handlers(fw_cfg:get_key(http_handlers)).


load_handlers([]) ->
    ?LOG_INFO("finished loading request web handlers", []);
load_handlers([H|T]) ->
    try
        ?LOG_DEBUG("loading request web handler module: ~p", [H]),
        H:start()
    catch
        _:_ ->
            ?LOG_ERROR("failed to load web handler module: ~p", [H])
    end,
    load_handlers(T).


uri_register(Path, Module) ->
    ?LOG_DEBUG("uri_register() found something else: ~9999p by ~p", [Path, Module]),
    uri_register(Path, Module, all).
uri_register([$/|Path], Module, Methods) ->
    ?LOG_DEBUG("uri_register() stripped leading /: ~9999p", [Path]),
    uri_register(Path, Module, Methods);
uri_register(Path, Module, Methods) ->
    ?LOG_INFO("registering uri handler: ~9999p by ~p for ~9999p", [Path, Module, Methods]),
    fw_cfg:add_key(uri_handlers, {Path, Module, Methods}).


handle_init() ->
    init.


handle_data(Socket, Packet, init) ->
    ?LOG_DEBUG("handle_data() begin initial web request processing", []),
    read_packet(erlang:decode_packet(http_bin, Packet, []), #req{s = Socket});
handle_data(_Socket, Packet, #req{uri = #uri{path = Path}, body = Body, left = Left} = Req) when Left - size(Packet) =< 0 ->
    ?LOG_DEBUG("handle_data() reached end of data transmission for request: ~9999p", [Path]),
    handle_method(tl(Path), Req#req{body = <<Body/bytes, Packet/bytes>>, left = 0});
handle_data(_Socket, Packet, #req{uri = #uri{path = Path}, body = Body, left = Left} = Req) when Left - size(Packet) > 0 ->
    ?LOG_DEBUG("handle_data() received partial for request: ~9999p", [Path]),
    Req#req{body = <<Body/bytes, Packet/bytes>>, left = Left - size(Packet)}.


read_packet({ok, {http_request, Method, Request, Vsn}, Packet}, Req) ->
    {Uri, Qry} = parse_request(Request),
    ?LOG_DEBUG("read_packet() extracted request: ~9999p: ~9999p", [Method, Request]),

    case lists:filter(fun({E, _, _}) -> E =:= hd(Uri#uri.path) end, fw_cfg:get_key(uri_handlers, [])) of
        [{_, Module, Methods}|_] ->
            case Methods =:= all orelse lists:member(Method, Methods) of
                true ->
                    ?LOG_INFO("found handler for request: ~9999p: ~p", [Uri#uri.path, Module]),
                    read_packet(erlang:decode_packet(httph_bin, Packet, []), Req#req{
                        method = http_method(Method), module = Module, uri = Uri, qry = Qry, vsn = Vsn
                    });
                false ->
                    ?LOG_DEBUG("read_packet() found unimplemented method ~p for request: ~9999p", [Method, Uri#uri.path]),
                    send_packet(Req#req.s, 501),
                    disconnect
            end;
        _ ->
            ?LOG_DEBUG("read_packet() found unimplemented request: ~9999p", [Uri#uri.path]),
            send_packet(Req#req.s, 501),
            disconnect
    end;
read_packet({ok, {http_header, _, 'Content-Length' = Key, _, Val}, Packet}, #req{head = Head} = Req) ->
    ?LOG_DEBUG("read_packet() extracted length: ~9999p: ~9999p", [Key, Val]),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), Req#req{head = [{Key, Val}|Head], left = list_to_integer(binary_to_list(Val))});
read_packet({ok, {http_header, _, <<"Expect">> = Key, _, <<"100-continue">> = Val}, Packet}, #req{head = Head} = Req) ->
    ?LOG_DEBUG("read_packet() extracted expect: ~9999p: ~9999p", [Key, Val]),
    send_packet(Req#req.s, 100),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), Req#req{head = [{Key, Val}|Head]});
read_packet({ok, {http_header, _, Key, _, Val}, Packet}, #req{head = Head} = Req) ->
    ?LOG_DEBUG("read_packet() extracted header: ~9999p: ~9999p", [Key, Val]),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), Req#req{head = [{Key, Val}|Head]});
read_packet({ok, http_eoh, <<>>}, #req{uri = #uri{path = Path}, left = 0} = Req) ->
    ?LOG_DEBUG("read_packet() reached end of headers for reqest: ~9999p", [Path]),
    handle_method(tl(Path), Req);
read_packet({ok, http_eoh, Body}, #req{uri = #uri{path = Path}, left = Left} = Req) when size(Body) =:= Left ->
    ?LOG_DEBUG("read_packet() reached end of headers, read full body: ~9999p", [Path]),
    handle_method(tl(Path), Req#req{left = 0, body = Body});
read_packet({ok, http_eoh, Body}, #req{uri = #uri{path = Path}, left = Left} = Req) ->
    ?LOG_DEBUG("read_packet() reached end of headers, reading body: ~9999p", [Path]),
    Req#req{body = Body, left = Left - size(Body)};
read_packet({ok, {http_error, Err}, _}, Req) ->
    ?LOG_DEBUG("read_packet() extracted bad http packet: ~9999p", [Err]),
    send_packet(Req#req.s, 400),
    disconnect.


send_packet(Socket, Code) ->
    send_packet(Socket, Code, <<>>).
send_packet(Socket, Code, Body) ->
    send_packet(Socket, Code, [{"Content-type", "text/html"}], Body).
send_packet(Socket, Code, Head, Body) ->
    Binary = try
        iolist_to_binary(Body)
    catch
        error:badarg ->
            ?LOG_ERROR("invalid iolist, using io_lib:format/2: ~9999p", [Body]),
            iolist_to_binary(io_lib:format("~p", [Body]))
    end,
    Packet = [
        "HTTP/1.1 ", http_response(Code), "\r\n",
        [ [Key, ": ", Val, "\r\n"] || {Key, Val} <- [{"Content-length", integer_to_list(byte_size(Binary))}|Head] ],
        "\r\n", Binary
    ],
    ?LOG_DEBUG("sending response to client: ~p: ~9999p", [Socket, Packet]),
    gen_tcp:send(Socket, Packet).


http_method('GET')    -> handle_get;
http_method('HEAD')   -> handle_head;
http_method('PUT')    -> handle_put;
http_method('POST')   -> handle_post;
http_method('DELETE') -> handle_delete;
http_method(_)        -> handle_other.


http_response(100) -> "100 Continue";
http_response(200) -> "200 OK";
http_response(201) -> "201 Created";
http_response(400) -> "400 Bad Request";
http_response(401) -> "401 Forbidden";
http_response(403) -> "403 Unauthorized";
http_response(404) -> "404 Not Found";
http_response(409) -> "409 Conflict";
http_response(500) -> "500 Internal Server Error";
http_response(501) -> "501 Not Implemented";
http_response(503) -> "503 Unavailable";
http_response(504) -> "504 Gateway Timeout";
http_response(Code) -> integer_to_list(Code).


parse_request({abs_path, Path}) ->
    parse_request(#uri{path = Path});
parse_request({absoluteURI, Scheme, Host, Port, Path}) ->
    parse_request(#uri{scheme = Scheme, host = Host, port = Port, path = Path});
%% TODO: add handler for unknown scheme URIs
%parse_request({scheme, Scheme, URI}) ->
%    split uri into components
%    parse_request(#uri{scheme = Scheme, host = Host, port = Port, path = Path});
parse_request(#uri{path = Path} = Uri) ->
    {match, [P, Q]} = re:run(Path, "(?<URI>[^?]+)(?:\\?(?<QRY>.+))?", [{capture, [1, 2], binary}]),
    {Uri#uri{path = parse_uripath(P)}, parse_qstring(Q)}.


hex_to_int(C) when C >= $0 andalso C =< $9 ->
    C - $0;
hex_to_int(C) when C >= $A andalso C =< $F ->
    C - $A + 10;
hex_to_int(C) when C >= $a andalso C =< $f ->
    C - $a + 10.


parse_uripath(P) ->
    parse_uripath(P, "", []).

parse_uripath(<<>>, "", Acc) ->
    lists:reverse(Acc);
parse_uripath(<<>>, Str, Acc) ->
    lists:reverse([lists:reverse(Str)|Acc]);
parse_uripath(<<$/, P/bytes>>, "", []) ->
    parse_uripath(P, "", []);
parse_uripath(<<$/, P/bytes>>, Str, Acc) ->
    parse_uripath(P, "", [lists:reverse(Str)|Acc]);
parse_uripath(<<$%, C1:8, C2:8, P/bytes>>, Str, Acc) ->
    C = hex_to_int(C1) bsl 4 + hex_to_int(C2),
    parse_uripath(P, [C|Str], Acc);
parse_uripath(<<C:8, P/bytes>>, Str, Acc) ->
    parse_uripath(P, [C|Str], Acc).


parse_qstring(Q) ->
    parse_qstring(Q, <<>>, []).

parse_qstring(<<>>, <<>>, []) ->
    [];
parse_qstring(<<>>, Str, [{Key}|Acc]) ->
    [{Key, Str}|Acc];
parse_qstring(<<>>, Str, Acc) ->
    [{<<>>, Str}|Acc];
parse_qstring(<<$%, C1:8, C2:8, Q/bytes>>, Str, Acc) ->
    C = hex_to_int(C1) bsl 4 + hex_to_int(C2),
    parse_qstring(Q, <<Str/bytes, C:8>>, Acc);
parse_qstring(<<$=, Q/bytes>>, Str, Acc) ->
    parse_qstring(Q, <<>>, [{Str}|Acc]);
parse_qstring(<<$&, Q/bytes>>, Str, [{Key}|Acc]) ->
    parse_qstring(Q, <<>>, [{Key, Str}|Acc]);
parse_qstring(<<$&, Q/bytes>>, Str, Acc) ->
    parse_qstring(Q, <<>>, [{<<>>, Str}|Acc]);
parse_qstring(<<C:8, Q/bytes>>, Str, Acc) ->
    parse_qstring(Q, <<Str/bytes, C:8>>, Acc).


handle_method(Uri, #req{s = S, method = Method, module = Module} = Req) ->
    case catch Module:Method(Uri, Req) of
        {Code, Head, Body} when is_integer(Code) andalso is_list(Head) andalso is_binary(Body) ->
            send_packet(S, Code, Head, Body);
        {Code, Body} when is_integer(Code) andalso is_binary(Body) ->
            send_packet(S, Code, Body);
        Other ->
            send_packet(S, 500, Other)
    end,
    %% TODO: handle keep-alive connections
    disconnect.
