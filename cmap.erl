-module(cmap).
-author('olivier@biniou.info').

-export([new/1]).


new(File) ->
    {ok, Bin} = file:read_file(File),
    P = parse(Bin),
    list_to_tuple(P).


parse(Bin) ->
    parse(Bin, []).
parse(<<>>, Acc) ->
    lists:reverse(Acc);
parse(<<R,G,B,_A, Rest/binary>>, Acc) ->
    RGB = {R, G, B},
    parse(Rest, [RGB|Acc]).


