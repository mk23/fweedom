-module(ts_log).

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([set_level/0, set_level/1, write_log/5]).

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-include("ts.hrl").

-record(log_level, {index, label, prefix, method}).
-define(LOG_LEVELS, [
    #log_level{index = 0, method = log_none},
    #log_level{index = 1, prefix = "(c)", method = log_crit},
    #log_level{index = 2, prefix = "(e)", method = log_error},
    #log_level{index = 3, prefix = "(w)", method = log_warn},
    #log_level{index = 4, prefix = "(i)", method = log_info},
    #log_level{index = 5, prefix = "(d)", method = log_debug}
]).

-record(state, {fd = 2, fn = "", error = ""}).

start() ->
    ChildSpec = {?MODULE,
        {?MODULE, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [?MODULE]
    },
    supervisor:start_child(ts_sup, ChildSpec),
    set_level().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_level() ->
    set_level(ts_cfg:get_key(log_level)).

set_level(none)          -> set_level(0);
set_level(quiet)         -> set_level(0);
set_level(silent)        -> set_level(0);
set_level(crit)          -> set_level(1);
set_level(critical)      -> set_level(1);
set_level(err)           -> set_level(2);
set_level(error)         -> set_level(2);
set_level(warn)          -> set_level(3);
set_level(warning)       -> set_level(3);
set_level(info)          -> set_level(4);
set_level(information)   -> set_level(4);
set_level(informational) -> set_level(4);
set_level(debug)         -> set_level(5);
set_level(verbose)       -> set_level(5);

set_level(Limit) ->
    LogFun = fun
        (MinLevel, LogLevel) when MinLevel >= LogLevel#log_level.index ->
            io_lib:format("
                ~p(File, Line, Format, Params) ->
                    ts_log:write_log(File, Line, ~p, Format ++ \"~~n\", Params).
                ", [LogLevel#log_level.method, LogLevel#log_level.prefix]);
        (_, LogLevel) ->
            io_lib:format("
                ~p(_, _, _, _) ->
                    ok.
                ", [LogLevel#log_level.method])
    end,

    ModSrc = lists:flatten("
        -module(" ++ atom_to_list(?LOG_MODULE) ++ ").
        -export([
            log_crit/4,
            log_error/4,
            log_warn/4,
            log_info/4,
            log_debug/4
        ]).
        " ++ [LogFun(Limit, Level) || Level <- ?LOG_LEVELS, Level#log_level.index =/= 0]
    ),

    try build_logger(ModSrc) of
        {module, ?LOG_MODULE} ->
            ?LOG_INFO("successfully built and loaded the dynamic logger", [])
    catch
        Type:Error ->
            ts_log:write_log(?MODULE, ?LINE, "(c)", "failed to compile logger: ~p:~p", [Type, Error])
    end.

build_logger(String) ->
    {ok, Toks, _} = erl_scan:string(String),
    {ok, _, Code} = compile:forms(parse_tokens(Toks, [], [])),
    code:load_binary(?LOG_MODULE, atom_to_list(?LOG_MODULE) ++ ".erl", Code).

parse_tokens([{dot,_} = Dot|[]], ExpAcc, ModAcc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([Dot|ExpAcc])),
    lists:reverse([Form|ModAcc]);
parse_tokens([{dot,_} = Dot|T], ExpAcc, ModAcc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([Dot|ExpAcc])),
    parse_tokens(T, [], [Form|ModAcc]);
parse_tokens([H|T], ExpAcc, ModAcc) ->
    parse_tokens(T, [H|ExpAcc], ModAcc).


write_log(File, Line, Prefix, Format, Params) ->
    gen_server:cast(?MODULE, {write_log, File, Line, Prefix, Format, Params}).


init([]) ->
    FN = ts_cfg:get_key(log_file),
    case file:open(FN, [append]) of
        {ok, FD} ->
            {ok, #state{fn = FN, fd = FD}};
        {error, Reason} ->
            {ok, #state{fn = FN, fd = standard_error, error = Reason}}
    end.

handle_cast({write_log, File, Line, Prefix, Format, Params}, State) ->
    {_, _, Ms} = Now = erlang:now(),
    {{Yr, Mo, Dy},{Hr, Mi, Se}} = calendar:now_to_local_time(Now),
    io:fwrite(State#state.fd, "[~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B] ~s ~s:~B - " ++ Format,
        [Yr, Mo, Dy, Hr, Mi, Se, Ms div 1000, Prefix, File, Line] ++ Params
    ),
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_call(Req, _From, State) ->
    {reply, Req, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {ok, Reason}.
