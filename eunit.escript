#!/usr/bin/env escript

main([]) ->
    io:format("usage: eunit.escript <ebin> <reports> <module1> [module2 [...]]~n", []),
    halt(1);
main(Mods) ->
    Fun = fun(Str) ->
        Mod = list_to_atom(Str ++ "_tests"),
        case code:ensure_loaded(Mod) of
            {module, Mod} ->
                io:format("~s:~n", [Str]),
                eunit:test(list_to_atom(Str), [{report, {eunit_surefire, [{dir, "reports"}]}}]);
            {error,nofile} ->
                io:format("~s: no tests~n", [Str])
        end
    end,

    code:add_patha("ebin"),
    lists:foreach(Fun, Mods).
