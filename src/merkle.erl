-module(merkle).

%% API exports
-export([build/2, build/1, combine/2, to_leaf/1]).

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
    build_tree(List).

%%====================================================================
%% Internal functions
%%====================================================================
build_tree([Root = #inner{}]) ->
    Root;
build_tree(List) ->
    UpperLevel = lists:reverse(ccombine(List, [])),
    case length(UpperLevel) of
        1 -> hd(UpperLevel);
        _ -> build_tree(UpperLevel)
    end.

combine(L = #leaf{hashkey = LHash}, R = #leaf{hashkey = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
combine(L = #inner{hash = LHash}, R = #leaf{hashkey = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
combine(L = #leaf{hashkey = LHash}, R = #inner{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
combine(L = #inner{hash = LHash}, R = #inner{hash = RHash}) ->
    #inner{left = L, right = R, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)}.

ccombine([], Acc) ->
    Acc;
ccombine([X], Acc) ->
    [X | Acc];
ccombine([X, Y | T], Acc) ->
    NAcc = [combine(X, Y) | Acc],
    ccombine(T, NAcc).

to_leaf({Key, Value}) ->
    #leaf{key = Key, hashkey = crypto:hash(?HASH, <<Key/binary, Value/binary>>)}.
