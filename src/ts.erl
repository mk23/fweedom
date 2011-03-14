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

%% TODO: Get a real logger with real log levels and real checking of log levels

-module(ts).
-include_lib("eunit/include/eunit.hrl").

-export([start/0, stop/0]).

-include("ts.hrl").

%% @spec start() -> ok
%% @doc Starts the main application.
start() ->
    application:start(sasl),
    application:start(mnesia),
    application:start(ts, permanent).

%% @spec stop() -> ok
%% @doc Stops the main application.
stop() ->
    application:stop(ts).
