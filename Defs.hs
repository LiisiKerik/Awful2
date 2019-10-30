--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Defs where
  import Cats_1
  import Classes_0
  import Classes_1
  import Data.Map
  import Data.Set
  import Datas_0
  import Defs_and_instances_0
  import Defs_and_instances_1
  import Naming
  import Standard
  import Tokenise
  instances :: Map' (Map' Inst)
  instances =
    Data.Map.fromList [("Ord", Data.Map.fromList [("Int", Inst [] [])]), ("Ring", Data.Map.fromList [("Int", Inst [] [])])]
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  type_cat_2 ::
    (
      (
        String,
        Map' Kind,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' PConstructor,
        Map' Constructor,
        Map' Type_2,
        Map' Prom_alg) ->
      Cat_6 ->
      Err (Map' Expression_2))
  type_cat_2 (a, o, u, m', z', j, f7, k7, l, f4) (Cat_6 b (p, d) n (h, i)) =
    let
      f3 = Prelude.foldl Application_kind_1 (Name_kind_1 p) (Name_kind_1 <$> d)
      x = arrow_type f3
      y = Data.Map.union o (Data.Map.fromList ((\y' -> (y', Star_kind)) <$> d))
      s' m7 fj u1 n7 =
        type_expr
          (m7 ++ " " ++ p ++ location' (Location_1 a b))
          fj
          a
          (f4, f7, k7, l)
          u1
          z'
          n7
          m'
          (u, y)
          (Prelude.foldl (\m3 -> \t5 -> Data.Map.insert t5 (Cat_4 [] []) m3) j n)
          b
      a8 u3 = new_typevar f4 (u3, Data.Set.empty) f3
      ((x0, _), z0, _) = a8 0
      ((w1, _), z1, _) = a8 x0
      ((x2, _), z2, _) = a8 w1
    in
      (
        (\j7 -> \j8 -> Data.Map.fromList [("compose " ++ p, j7), ("id " ++ p, j8)]) <$>
        s' "compose" (function_type (x z0 z1) (function_type (x z2 z0) (x z2 z1))) h x2 <*>
        s' "id" (x z0 z0) i x0)
  type_cats_2 ::
    (
      (
        String,
        Map' Kind,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' PConstructor,
        Map' Constructor,
        Map' Type_2,
        Map' Prom_alg) ->
      [Cat_6] ->
      Err (Map' Expression_2))
  type_cats_2 a b =
    case b of
      [] -> Right Data.Map.empty
      d : e -> Data.Map.union <$> type_cat_2 a d <*> type_cats_2 a e
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