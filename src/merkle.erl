-module(merkle).

%% API exports
-export([build/2, build/1]).

-define(HASH, sha256).

-record(leaf, {key, hash}).
-record(inner, {hash, left, right}).

%%====================================================================
%% API functions
%%====================================================================
build(A, B) ->
    L = [{term_to_binary(X), term_to_binary(X)} || X <- lists:seq(A, B)],
    build(L).

build(L) ->
    List = [to_leaf(X) || X <- lists:keysort(1, L)],
    build_tree(List).

%%====================================================================
%% Internal functions
%%====================================================================
build_tree([Root = #inner{}]) ->
    Root;
build_tree(List) ->
    UpperLevel = lists:reverse(combine(List, [])),
    build_tree(UpperLevel).

combine([], Acc) ->
    Acc;
combine([X], Acc) ->
    [X | Acc];
combine([X, Y | T], Acc) ->
    combine(T, [to_inner(X, Y) | Acc]).

to_leaf({Key, Value}) ->
    #leaf{key = Key, hash = crypto:hash(?HASH, <<Key/binary, Value/binary>>)}.

to_inner(L = #leaf{hash = LHash}, R = #leaf{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #inner{hash = LHash}, R = #leaf{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #leaf{hash = LHash}, R = #inner{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #inner{hash = LHash}, R = #inner{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)}.
