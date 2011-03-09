-module(uh_user).

-export([start/0]).
-export([handle_get/1, handle_post/1]).

-include("ts.hrl").

start() ->
    web_server:uri_register("user", ?MODULE, ['GET', 'POST']).

handle_get(Stuff) ->
    ?LOG_DEBUG("handling user get request: ~p", [Stuff]).

handle_post(Stuff) ->
    ?LOG_DEBUG("handling user post request: ~p", [Stuff]).
