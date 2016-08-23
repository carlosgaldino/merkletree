-module(merkle).

%% API exports
-export([build/2, build/1, chunk/1, combine/2, to_leaf/1]).

-define(HASH, sha256).

-record(leaf, {key, hashkey}).
-record(inner, {hash, left, right}).

%%====================================================================
%% API functions
%%====================================================================
build(A, B) ->
    L = [{term_to_binary(X), term_to_binary(X)} || X <- lists:seq(A, B)],
    build(L).

build(L) ->
    List = [to_leaf(X) || X <- lists:keysort(1, L)],
    Chunks = chunk(List),
    Tree = build_tree(Chunks),
    Tree.

%%====================================================================
%% Internal functions
%%====================================================================
build_tree([Root = #inner{}]) ->
    Root;
build_tree(List) ->
    UpperLevel = [combine(X, Y) || [X, Y] <- List],
    case length(UpperLevel) of
        1 -> hd(UpperLevel);
        _ -> build_tree(chunk(UpperLevel))
    end.

combine(L = #leaf{hashkey = LHash}, R = #leaf{hashkey = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
combine(L = #inner{hash = LHash}, R = #inner{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)}.

chunk(List) ->
    lists:reverse(chunk(List, [])).

chunk([X], Acc) ->
    [[X, X] | Acc];
chunk([X, Y], Acc) ->
    [[X, Y] | Acc];
chunk([X, Y | T], Acc) ->
    chunk(T, [[X, Y] | Acc]).

to_leaf({Key, Value}) ->
    #leaf{key = Key, hashkey = crypto:hash(?HASH, <<Key/binary, Value/binary>>)}.
