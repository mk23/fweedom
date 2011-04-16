%% @author Max Kalika <max.kalika+telephone@gmail.com>
%% @copyright 2011
%% @version {@version}
%% @since 1.0.0
%%
%% @doc telephone-server - Backend server for the Telephone domino game.
%%   This module provides the main starting point for the server.
%%   The application can be started <br/><br/>
%%   <code>erl -noshell -noinput -sname &lt;nodename&gt; -s ts</code>
%% @end

-module(ts).
-include_lib("eunit/include/eunit.hrl").

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Starts the main application.
start() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,

    application:start(sasl, permanent),
    application:start(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),

    application:start(fw, permanent).

%% @spec stop() -> ok
%% @doc Stops the main application.
stop() ->
    application:stop(fw).
