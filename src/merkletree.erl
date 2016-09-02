%%% @doc Merkle Tree is a data structure where every non-leaf node contains the
%%% hash of the labels of its child nodes, and the leaves have their own values
%%% (or key/value pair) hashed. Because of this characteristic, Merkle Trees are
%%% used to verify that two or more parties have the same data without
%%% exchanging the entire data collection. For more information about Merkle
%%% Trees and other use cases you can visit its Wikipedia article: [https://en.wikipedia.org/wiki/Merkle_tree]
%%%
%%% This module implements a binary Merkle Tree that is built based on a list of
%%% `{Key, Value}' pairs. The tree is sorted but might not be balanced.
%%%
%%% ```
%%%                                           ┌───────────────┐
%%%                                           │     Root      │
%%%                         ┌─────────────────│Hash(AA1 + BB2)│───────────────┐
%%%                         │                 └───────────────┘               │
%%%                         │                                                 │
%%%                         │                                                 │
%%%                         │                                                 │
%%%                         │                                                 │
%%%                  ┌─────────────┐                                   ┌─────────────┐
%%%                  │     AA1     │                                   │     BB2     │
%%%             ┌────│Hash(A1 + B1)│──────┐                         ┌──│Hash(C1 + D1)│────────┐
%%%             │    └─────────────┘      │                         │  └─────────────┘        │
%%%             │                         │                         │                         │
%%%             │                         │                         │                         │
%%%       ┌───────────┐             ┌───────────┐             ┌───────────┐             ┌───────────┐
%%%       │    A1     │             │    B1     │             │    C1     │             │    D1     │
%%%     ┌─│Hash(A + B)│─┐         ┌─│Hash(C + D)│─┐         ┌─│Hash(E + F)│─┐         ┌─│Hash(G + H)│─┐
%%%     │ └───────────┘ │         │ └───────────┘ │         │ └───────────┘ │         │ └───────────┘ │
%%%     │               │         │               │         │               │         │               │
%%%     │               │         │               │         │               │         │               │
%%% ┌───────┐       ┌───────┐ ┌───────┐       ┌───────┐ ┌───────┐       ┌───────┐ ┌───────┐       ┌───────┐
%%% │   A   │       │   B   │ │   C   │       │   D   │ │   E   │       │   F   │ │   G   │       │   H   │
%%% │Hash(A)│       │Hash(B)│ │Hash(C)│       │Hash(D)│ │Hash(E)│       │Hash(F)│ │Hash(G)│       │Hash(H)│
%%% └───────┘       └───────┘ └───────┘       └───────┘ └───────┘       └───────┘ └───────┘       └───────┘
%%% '''
%%%
%%% Every leaf node will have its `left' and `right' pointers pointing to
%%% `nil', and it will contain a hash based on the key and value. The inner
%%% nodes will point to their respective leaf nodes children, and its hash will
%%% be `Hash(LeftHash + RightHash)'.
%%% @end
-module(merkletree).

-define(HASH, sha256).

-record(inner, {key :: key() | 'undefined',
                hash :: hash(),
                height :: non_neg_integer(),
                min_key :: key(),
                max_key :: key(),
                left :: tree(),
                right :: tree()}).

-type inner() :: #inner{}.
-type tree() :: inner() | 'nil'.

-type key() :: binary().
-type value() :: binary().
-type hash() :: binary().

%% API exports
-export([build/1, diff/2, keys/1]).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Creates a tree from a list of `{Key, Value}' pairs.
-spec build([{key(), value()}]) -> tree().
build(L) ->
    List = [to_inner(X) || X <- lists:keysort(1, L)],
    build_tree(List).

%% @doc Returns the list of `Key' that are different between the given trees.
-spec diff(tree(), tree()) -> [key()].
diff(T1, T2) ->
    List = remove_equal_elements(dirty_diff(T1, T2), sets:new()),
    lists:usort([X || {X, _} <- List]).

%% @doc Returns a sorted list of all keys from the given tree.
-spec keys(tree()) -> [key()].
keys(Tree) ->
    [K || {K, _} <- lists:usort(extract_keys(Tree))].

%%====================================================================
%% Internal functions
%%====================================================================
-spec build_tree([tree()]) -> tree().
build_tree([]) ->
    'nil';
build_tree([Root]) ->
    Root;
build_tree(List) ->
    UpperLevel = lists:reverse(combine(List, [])),
    build_tree(UpperLevel).

-spec combine([tree()], [tree()]) -> [tree()].
combine([], Acc) ->
    Acc;
combine([X], Acc) ->
    [X | Acc];
combine([X, Y | T], Acc) ->
    combine(T, [to_inner(X, Y) | Acc]).

-spec to_inner({key(), value()}) -> inner().
to_inner({Key, Value}) ->
    #inner{key = Key, min_key = Key, max_key = Key, height = 0, left = 'nil', right = 'nil', hash = crypto:hash(?HASH, <<Key/binary, Value/binary>>)}.

-spec to_inner(inner(), inner()) -> inner().
to_inner(L = #inner{hash = LHash, min_key = MinKey, height = LHeight}, R = #inner{hash = RHash, max_key = MaxKey, height = RHeight}) ->
    Height = max(LHeight, RHeight) + 1,
    #inner{left = L, right = R, height = Height, min_key = MinKey, max_key = MaxKey, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)}.

-spec extract_keys(tree()) -> [{key(), hash()}].
extract_keys('nil') ->
    [];
extract_keys(#inner{key = Key, hash = Hash, left = 'nil', right = 'nil'}) ->
    [{Key, Hash}];
extract_keys(#inner{left = Left, right = Right}) ->
    lists:flatten([extract_keys(Left), extract_keys(Right)]).

-spec remove_equal_elements([{key(), hash()}], sets:set()) -> [{key(), hash()}].
remove_equal_elements([], Set) ->
    sets:to_list(Set);
remove_equal_elements([H|T], Set) ->
    case sets:is_element(H, Set) of
        true -> remove_equal_elements(T, sets:del_element(H, Set));
        false -> remove_equal_elements(T, sets:add_element(H, Set))
    end.

%% This function returns the list of `{Key, Hash}' pairs that are different
%% between the given trees.
%%
%% The idea is to compare the hashes and if they are different the next
%% iteration only goes to the possible branch that might contain the entire tree.
%%
%% Let's call the given trees T1 and T2, respectively. If T1 is bigger (T1#height > T2#height) we switch them.
%%
%% When T1 might be entirely contained in a branch of T2, the diff continues
%% solely on that branch and the keys from the other T2 branch are collected.
%%
%% If T1 keys overlap between the two left and right branches of T2 we continue the diff by each branch.
-spec dirty_diff(tree(), tree()) -> [{key(), hash()}].
dirty_diff(T, T) ->
    [];
dirty_diff('nil', T2) ->
    lists:flatten([extract_keys(T2)]);
dirty_diff(T1, 'nil') ->
    lists:flatten([extract_keys(T1)]);
dirty_diff(T1 = #inner{hash = _Hash1, height = LHeight}, T2 = #inner{hash = _Hash2, height = RHeight}) when LHeight > RHeight ->
    dirty_diff(T2, T1);
dirty_diff(Leaf = #inner{left = 'nil', right = 'nil'}, Tree = #inner{}) ->
    Diff = case contained_branch(Leaf, Tree) of
               left -> [dirty_diff(Leaf, Tree#inner.left), extract_keys(Tree#inner.right)];
               right -> [dirty_diff(Leaf, Tree#inner.right), extract_keys(Tree#inner.left)];
               none -> [extract_keys(Leaf), extract_keys(Tree)]
           end,
    lists:flatten(Diff);
dirty_diff(T1 = #inner{hash = _Hash1}, T2 = #inner{hash = _Hash2}) ->
    Diff = case contained_branch(T1, T2) of
               left -> [dirty_diff(T1, T2#inner.left), extract_keys(T2#inner.right)];
               right -> [dirty_diff(T1, T2#inner.right), extract_keys(T2#inner.left)];
               none -> [dirty_diff(T1#inner.left, T2#inner.left), dirty_diff(T1#inner.right, T2#inner.right)]
           end,
    lists:flatten(Diff).

-spec contains(tree(), tree()) -> boolean().
contains(#inner{key = Key, left = 'nil', right = 'nil'}, #inner{min_key = MinKey}) ->
    MinKey >= Key;
contains(#inner{min_key = MinKey1, max_key = MaxKey1}, #inner{min_key = MinKey2, max_key = MaxKey2}) when MinKey1 >= MinKey2, MaxKey1 =< MaxKey2 ->
    true;
contains(_T1, _T2) ->
    false.

-spec contained_branch(tree(), tree()) -> 'left' | 'right' | 'none'.
contained_branch(#inner{max_key = MaxKey1, left = #inner{}, right = #inner{}}, #inner{min_key = MinKey2}) when MaxKey1 =< MinKey2 ->
    none;
contained_branch(T1 = #inner{}, #inner{left = Left2, right = Right2}) ->
    case contains(T1, Left2) of
        true -> left;
        false -> case contains(T1, Right2) of
                     true -> right;
                     false -> none
                 end
    end.
