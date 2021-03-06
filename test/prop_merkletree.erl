-module(prop_merkletree).
-include_lib("proper/include/proper.hrl").

prop_equal_trees() ->
    ?FORALL(L, keyvals(),
            begin
                T1 = merkletree:build(L),
                T2 = merkletree:build(L),
                merkletree:diff(T1, T2) =:= []
            end).

prop_partial_diff() ->
    ?FORALL({KV1, KV2}, diff_keyvals(),
            begin
                Keys = [K || {K, _} <- KV2],
                T1 = merkletree:build(KV1),
                T2 = merkletree:build(KV1 ++ KV2),
                Diff = merkletree:diff(T1, T2),
                Diff =:= merkletree:diff(T2, T1)
                    andalso
                    Diff =:= lists:sort(Keys)
            end).

prop_total_diff() ->
    ?FORALL({KV1, KV2}, non_empty_diff_keyvals(),
            begin
                K1 = [K || {K, _} <- KV1],
                K2 = [K || {K, _} <- KV2],
                Keys = lists:merge(K1, K2),
                T1 = merkletree:build(KV1),
                T2 = merkletree:build(KV2),
                Diff = merkletree:diff(T1, T2),
                Diff =:= merkletree:diff(T2, T1)
                    andalso
                    Diff =:= lists:sort(Keys)
            end).

prop_keys() ->
    ?FORALL(KV, keyvals(),
            begin
                Keys = [K || {K, _} <- KV],
                Tree = merkletree:build(KV),
                merkletree:keys(Tree) =:= lists:sort(Keys)
            end).

keyvals() ->
    ?SUCHTHAT(KV, list({binary(), binary()}),
              begin
                  UList = lists:usort(KV),
                  length(UList) =:= length(KV)
              end
             ).

kv_non_repeating_keys() ->
    ?SUCHTHAT(KV, keyvals(),
              begin
                  UList = lists:ukeysort(1, KV),
                  length(UList) =:= length(KV)
              end
             ).

diff_keyvals() ->
    ?SUCHTHAT({KV1, KV2}, {kv_non_repeating_keys(), kv_non_repeating_keys()},
              begin
                  K1 = [K || {K, _} <- KV1],
                  K2 = [K || {K, _} <- KV2],
                  lists:all(fun(X) -> not lists:member(X, K2) end, K1)
                      andalso
                      length(lists:usort(K2)) =:= length(K2)
              end).

non_empty_diff_keyvals() ->
    ?SUCHTHAT({KV1, KV2}, diff_keyvals(), KV1 =/= [] andalso KV2 =/= []).
