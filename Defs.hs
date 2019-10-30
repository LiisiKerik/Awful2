--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Defs where
  import Cats_1
  import Cats_2
  import Classes_0
  import Classes_1
  import Data.Map
  import Datas_0
  import Defs_and_instances_0
  import Defs_and_instances_1
  import Naming
  import Standard
  import Tokenise
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  type_defs ::
    (
      String ->
      (Map' Kind, Map' Polykind, Map' Class_4, Map' Class_5, Map' Cat_4, Map' Constructor, Map' PConstructor, Map' Prom_alg) ->
      ([Def_3], [Cat_6]) ->
      (Map' (Type_2, Status), Map' (Map' Inst), Map' Expression_2, Map' ([String], Map' [(String, Nat)])) ->
      Err (Map' Type_2, Map' (Map' Inst), Map' Expression_2, Map' ([String], Map' [(String, Nat)])))
  type_defs a (u, f', q', u', i', m', f2, f5) (f, p') (r', n, o', s') =
    (
      type_defs_1 a u f f' r' q' u' i' (old' n, s') >>=
      \(v', w', (x', y')) ->
        (
          type_cats_2 (a, u, f', y', fmap fst <$> x', i', f2, m', fst <$> w', f5) p' >>=
          \a0 ->
            (
              (\b0 -> (rem_old w', rem_old' x', Data.Map.union (Data.Map.union o' a0) b0, y')) <$>
              type_defs_2 a v' (f5, f2, m', fst <$> w') (fmap fst <$> x') f' y' q' u i')))
--------------------------------------------------------------------------------------------------------------------------------