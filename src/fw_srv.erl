-module(fw_srv).

-behavior(gen_server).

%% API
-export([start/0, start_link/0]).
-export([new_socket/2, get_socket/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include("fw.hrl").

-record(state, {dict}).

start() ->
    ChildSpec = {?MODULE,
        {?MODULE, start_link, []},
        permanent,
        2000,
        worker,
        [?MODULE]
    },
    supervisor:start_child(fw_sup, ChildSpec).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


new_socket(Module, Socket) ->
    gen_server:cast(?MODULE, {new_socket, Module, Socket}).


get_socket(Module) ->
    gen_server:call(?MODULE, {get_socket, Module}).


init([]) ->
    ?LOG_DEBUG("started socket server", []),
    {ok, #state{dict = dict:new()}}.


handle_call({get_socket, Module}, _From, State) ->
    case catch dict:fetch(Module, State#state.dict) of
        Socket when is_port(Socket) ->
            ?LOG_DEBUG("fetched socket: ~p for ~p", [Socket, Module]),
            {reply, Socket, State};
        _ ->
            {reply, {error, not_found}, State}
    end.


handle_cast({new_socket, Module, Socket}, State) ->
    ?LOG_DEBUG("storing socket: ~p for ~p", [Socket, Module]),
    {noreply, #state{dict = dict:store(Module, Socket, State#state.dict)}}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    {ok, Reason}.
