-module(merkle).

-define(HASH, sha256).

-record(leaf, {key :: key(),
               hash :: hash()}).
-record(inner, {hash :: hash(),
                min_key :: key(),
                max_key :: key(),
                left :: non_empty_tree(),
                right :: non_empty_tree()}).

-type leaf() :: #leaf{}.
-type inner() :: #inner{}.
-type non_empty_tree() :: leaf() | inner().
-type tree() :: non_empty_tree() | 'nil'.

-type key() :: binary().
-type value() :: binary().
-type hash() :: binary().

%% API exports
-export([build_raw/1, build/2, build/1, diff/2]).

%%====================================================================
%% API functions
%%====================================================================
build(A, B) ->
    L = [{term_to_binary(X), term_to_binary(X)} || X <- lists:seq(A, B)],
    build(L).

build_raw(L) ->
    LL = [{term_to_binary(X), term_to_binary(X)} || X <- L],
    build(LL).

-spec build([{key(), value()}]) -> tree().
build(L) ->
    List = [to_leaf(X) || X <- lists:keysort(1, L)],
    build_tree(List).

-spec diff(tree(), tree()) -> [key()].
diff(T1, T2) ->
    List = remove_equal_elements(dirty_diff(T1, T2), sets:new()),
    lists:usort([X || {X, _} <- List]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec build_tree([{key(), value()}]) -> tree().
build_tree([]) ->
    'nil';
build_tree([Root]) ->
    Root;
build_tree(List) ->
    UpperLevel = lists:reverse(combine(List, [])),
    build_tree(UpperLevel).

-spec combine([non_empty_tree()], [non_empty_tree()]) -> [non_empty_tree()].
combine([], Acc) ->
    Acc;
combine([X], Acc) ->
    [X | Acc];
combine([X, Y | T], Acc) ->
    combine(T, [to_inner(X, Y) | Acc]).

-spec to_leaf({key(), value()}) -> leaf().
to_leaf({Key, Value}) ->
    #leaf{key = Key, hash = crypto:hash(?HASH, <<Key/binary, Value/binary>>)}.

-spec to_inner(non_empty_tree(), non_empty_tree()) -> inner().
to_inner(L = #leaf{hash = LHash, key = LKey}, R = #leaf{hash = RHash, key = RKey}) ->
    #inner{left = L, right = R, min_key = LKey, max_key = RKey, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #inner{hash = LHash, min_key = MinKey}, R = #leaf{hash = RHash, key = RKey}) ->
    #inner{left = L, right = R, min_key = MinKey, max_key = RKey, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #leaf{hash = LHash, key = LKey}, R = #inner{hash = RHash, max_key = MaxKey}) ->
    #inner{left = L, right = R, min_key = LKey, max_key = MaxKey, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)};
to_inner(L = #inner{hash = LHash, min_key = MinKey}, R = #inner{hash = RHash, max_key = MaxKey}) ->
    #inner{left = L, right = R, min_key = MinKey, max_key = MaxKey, hash = crypto:hash(?HASH, <<LHash/binary, RHash/binary>>)}.

-spec extract_keys(non_empty_tree()) -> [{key(), hash()}].
extract_keys(#leaf{key = Key, hash = Hash}) ->
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

-spec dirty_diff(tree(), tree()) -> [{key(), hash()}].
dirty_diff('nil', 'nil') ->
    [];
dirty_diff('nil', T2) ->
    lists:flatten([extract_keys(T2)]);
dirty_diff(T1, 'nil') ->
    lists:flatten([extract_keys(T1)]);
dirty_diff(#leaf{hash = _Hash}, #leaf{hash = _Hash}) ->
    [];
dirty_diff(#inner{hash = _Hash}, #inner{hash = _Hash}) ->
    [];
dirty_diff(T1 = #leaf{hash = _Hash1}, T2 = #leaf{hash = _Hash2}) ->
    [extract_keys(T1), extract_keys(T2)];
dirty_diff(T1 = #inner{hash = _Hash1, min_key = MinKey1, max_key = MaxKey1}, T2 = #inner{hash = _Hash2, min_key = MinKey2, max_key = MaxKey2}) when MaxKey1 > MaxKey2, MinKey1 >= MinKey2 ->
    dirty_diff(T2, T1);
dirty_diff(Tree = #inner{}, Leaf = #leaf{}) ->
    dirty_diff(Leaf, Tree);
dirty_diff(Leaf = #leaf{}, Tree = #inner{}) ->
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

-spec contains(non_empty_tree(), non_empty_tree()) -> boolean().
contains(#leaf{key = Key}, #inner{min_key = MinKey}) ->
    MinKey > Key;
contains(#inner{min_key = MinKey1, max_key = MaxKey1}, #inner{min_key = MinKey2, max_key = MaxKey2}) when MinKey1 >= MinKey2, MaxKey1 =< MaxKey2 ->
    true;
contains(_T1, _T2) ->
    false.

-spec contained_branch(non_empty_tree(), non_empty_tree()) -> 'left' | 'right' | 'none'.
contained_branch(#leaf{key = Key}, #inner{left = #leaf{key = Key}}) ->
    left;
contained_branch(#leaf{key = Key}, #inner{right = #leaf{key = Key}}) ->
    right;
contained_branch(#leaf{}, #inner{left = #leaf{}, right = #leaf{}}) ->
    none;
contained_branch(Leaf = #leaf{}, #inner{left = Left = #inner{}, right = Right = #inner{}}) ->
    case contains(Leaf, Right) of
        false -> contained_branch(Leaf, Left);
        true -> contained_branch(Leaf, Right)
    end;
contained_branch(#inner{max_key = MaxKey1}, #inner{min_key = MinKey2}) when MaxKey1 =< MinKey2 ->
    none;
contained_branch(T1 = #inner{}, #inner{left = Left2, right = Right2}) ->
    case contains(T1, Left2) of
        true -> left;
        false -> case contains(T1, Right2) of
                     true -> right;
                     false -> none
                 end
    end.
