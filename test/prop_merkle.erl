-module(prop_merkle).
-include_lib("proper/include/proper.hrl").

prop_equal_trees() ->
    ?FORALL(L, keyvals(),
            begin
                T1 = merkle:build(L),
                T2 = merkle:build(L),
                merkle:diff(T1, T2) =:= []
            end).

prop_partial_diff() ->
    ?FORALL({KV1, KV2}, diff_keyvals(),
            begin
                Keys = [K || {K, _} <- KV2],
                T1 = merkle:build(KV1),
                T2 = merkle:build(KV1 ++ KV2),
                Diff = merkle:diff(T1, T2),
                Diff =:= merkle:diff(T2, T1)
                    andalso
                    Diff =:= lists:sort(Keys)
            end).

prop_total_diff() ->
    ?FORALL({KV1, KV2}, non_empty_diff_keyvals(),
            begin
                K1 = [K || {K, _} <- KV1],
                K2 = [K || {K, _} <- KV2],
                Keys = lists:merge(K1, K2),
                T1 = merkle:build(KV1),
                T2 = merkle:build(KV2),
                Diff = merkle:diff(T1, T2),
                Diff =:= merkle:diff(T2, T1)
                    andalso
                    Diff =:= lists:sort(Keys)
            end).

keyvals() -> ?SUCHTHAT(KV, list({binary(), binary()}),
                       begin
                           UList = lists:ukeysort(1, KV),
                           length(UList) =:= length(KV)
                       end
                      ).

diff_keyvals() ->
    ?SUCHTHAT({KV1, KV2}, {keyvals(), keyvals()},
              begin
                  K1 = [K || {K, _} <- KV1],
                  K2 = [K || {K, _} <- KV2],
                  lists:all(fun(X) -> not lists:member(X, K2) end, K1)
                      andalso
                      length(lists:usort(K2)) =:= length(K2)
              end).

non_empty_diff_keyvals() ->
    ?SUCHTHAT({KV1, KV2}, diff_keyvals(), KV1 =/= [] andalso KV2 =/= []).
