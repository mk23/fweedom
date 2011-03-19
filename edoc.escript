#!/usr/bin/env escript

main([Version]) ->
    edoc:application(ts, "ebin", [
        {source_path, ["src"]},
        {dir, "docs"},
        {new, true},
        {todo, true},
        {private, true},
        {preprocess, true},
        {includes, ["include"]},
        {def, [{version, Version}]}
    ]);
main(_) ->
    io:format("usage: edoc.escript <Version>~n", []).
