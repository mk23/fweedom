%% @author Max Kalika <max.kalika+telenphone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%% @reference See
%%   <a href="http://www.erlang.org/doc/man/gen_tcp.html" target="_blank">Erlang TCP</a>
%%   documentation for more information.
%%
%% @doc telephone-server - HTTP requests handler
%%   This module provides functions to handle a subset of HTTP/1.1 requests and dispatches
%%   them to registered uri handlers implemented by other modules.

-module(web_server).

-export([handle_data/3]).
-export([uri_register/2, uri_register/3]).

-include("ts.hrl").

-record(uri, {scheme = http, host, port = ts_cfg:get_key(listen_port), path}).
-record(state, {s, method, module, uri = #uri{}, qry = [], vsn = {1, 0}, head = [], body = <<>>, left = 0}).

uri_register(Path, Module) ->
    uri_register(Path, Module, all).

uri_register([$/|_] = Path, Module, Methods) ->
    ?LOG_INFO("registered uri handler: ~p by ~p for ~p", [Path, Module, Methods]),
    ts_cfg:add_key(uri_handlers, <<Path/bytes>>, {Module, Methods});
uri_register(Path, Module, Methods) ->
    uri_register([$/|Path], Module, Methods).


handle_data(Socket, Packet, new) ->
    ?LOG_DEBUG("handle_data() begin new web request processing", []),
    read_packet(erlang:decode_packet(http_bin, Packet, []), #state{s = Socket});
handle_data(_Socket, Packet, #state{uri = #uri{path = Path}, body = Body, left = Left} = State) when Left - size(Packet) =< 0 ->
    ?LOG_DEBUG("handle_data() reached end of data transmission for request: ~p", [Path]),
    handle_method(State#state{body = <<Body/bytes, Packet/bytes>>, left = 0});
handle_data(_Socket, Packet, #state{uri = #uri{path = Path}, body = Body, left = Left} = State) when Left - size(Packet) > 0 ->
    ?LOG_DEBUG("handle_data() received partial for request: ~p", [Path]),
    State#state{body = <<Body/bytes, Packet/bytes>>, left = 0}.


read_packet({ok, http_eoh, <<>>}, #state{uri = #uri{path = Path}, left = 0} = State) ->
    ?LOG_DEBUG("read_packet() reached end of headers for reqest: ~p", [Path]),
    handle_method(State);
read_packet({ok, {http_request, Method, Request, Vsn}, Packet}, State) ->
    {Uri, Qry} = parse_request(Request),
    ?LOG_DEBUG("read_packet() extracted request: ~p:~p", [Method, Request]),
    case lists:keyfind(Uri#uri.path, 1, ts_cfg:get_mod(uri_handlers, [])) of
        {_, Module, Methods} ->
            case Methods =:= all orelse lists:member(Method, Methods) of
                true ->
                    ?LOG_DEBUG("read_packet() found handler for request: ~p: ~p", [Uri#uri.path, Module]),
                    read_packet(erlang:decode_packet(httph_bin, Packet, []), State#state{method = Method, module = Module, uri = Uri, qry = Qry, vsn = Vsn});
                false ->
                    ?LOG_DEBUG("read_packet() found unimplemented method ~p for request: ~p", [Method, Uri#uri.path]),
                    send_packet(State#state.s, 501),
                    gen_tcp:close(State#state.s)
            end;
        _ ->
            ?LOG_DEBUG("read_packet() found unimplemented request: ~p", [Uri#uri.path]),
            send_packet(State#state.s, 501),
            gen_tcp:close(State#state.s)
    end;
read_packet({ok, {http_header, _, 'Content-Length' = Key, _, Val}, Packet}, #state{head = Head} = State) ->
    ?LOG_DEBUG("read_packet() extracted length: ~p:~p", [Key, Val]),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), State#state{head = [{Key, Val}|Head], left = list_to_integer(binary_to_list(Val))});
read_packet({ok, {http_header, _, <<"Expect">> = Key, _, <<"100-continue">> = Val}, Packet}, #state{head = Head} = State) ->
    ?LOG_DEBUG("read_packet() extracted expect: ~p:~p", [Key, Val]),
    send_packet(State#state.s, 100),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), State#state{head = [{Key, Val}|Head]});
read_packet({ok, {http_header, _, Key, _, Val}, Packet}, #state{head = Head} = State) ->
    ?LOG_DEBUG("read_packet() extracted header: ~p:~p", [Key, Val]),
    read_packet(erlang:decode_packet(httph_bin, Packet, []), State#state{head = [{Key, Val}|Head]}).


send_packet(Socket, Code) ->
    send_packet(Socket, Code, <<>>).
send_packet(Socket, Code, Body) ->
    send_packet(Socket, Code, [{"Content-type", "text/html"}], Body).
send_packet(Socket, Code, Head, Body) ->
    Binary = iolist_to_binary(Body),
    Packet = [
        "HTTP/1.1 ", http_response(Code), "\r\n",
        [ [Key, ": ", Val, "\r\n"] || {Key, Val} <- [{"Content-length", integer_to_list(byte_size(Binary))}|Head] ],
        "\r\n", Binary
    ],
    ?LOG_DEBUG("sending response to client: ~p:~p", [Socket, Packet]),
    gen_tcp:send(Socket, Packet).


http_response(100) -> "100 Continue";
http_response(200) -> "200 OK";
http_response(201) -> "201 Created";
http_response(401) -> "401 Forbidden";
http_response(403) -> "403 Unauthorized";
http_response(404) -> "404 Not Found";
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
    {Uri#uri{path = P}, parse_qstring(Q)}.


hex_to_int(C) when C >= $0 andalso C =< $9 ->
    C - $0;
hex_to_int(C) when C >= $A andalso C =< $F ->
    C - $A + 10;
hex_to_int(C) when C >= $a andalso C =< $f ->
    C - $a + 10.


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

handle_method(#state{method = 'GET', module = Module} = State) ->
    Module:handle_get(State);
handle_method(#state{method = 'POST', module = Module} = State) ->
    Module:handle_post(State).
%% TODO: implement the rest of the handlers
