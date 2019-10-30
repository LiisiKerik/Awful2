--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas where
  import Cats_0
  import Cats_1
  import Data.Bifunctor
  import Datas_0
  import Datas_1
  import Datas_2
  import Naming
  import Standard
  import Tokenise
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
  int_type :: Type_1
  int_type = ntype "Int"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in
      Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pat_1 <$> c)
  type_datas ::
    (
      String ->
      ([Data_2], [Cat_3]) ->
      (
        Map' Polykind,
        Map' Constructor,
        Map' Type_2,
        Map' Kind,
        Map' Bool,
        Map' Prom_alg,
        Map' Cat_4,
        Map' PConstructor,
        Map' Expression_2) ->
      Err
        (
          (
            Map' (Polykind, Status),
            Map' (Constructor, Status),
            Map' (Type_2, Status),
            Map' (Kind, Status),
            Map' Bool,
            Map' (Prom_alg, Status),
            Map' (Cat_4, Status),
            Map' (PConstructor, Status),
            Map' Expression_2),
          [Cat_6]))
  type_datas a (b, c) (g, h, i, j, k, p, q, r, s) =
    (
      type_proms (Location_1 a) b q (j, g, s, k, i, p, h, r) >>=
      \((u, a', w, z, b', c', d', e'), y) ->
        (
          (,) <$> type_datas_1 a (fst <$> u, fst <$> c') y (a', w) <*> type_cats_0 (Location_1 a, fst <$> u) c (old q) >>=
          \(((f', g'), h'), (i', j')) ->
            (
              type_datas_2 (Location_1 a) h' (fst <$> f') (fst <$> u) (fst <$> i') (d', b') >>=
              \(k', l') ->
                (
                  first (\(m', n', o') -> (f', m', n', u, z, c', i', e', o')) <$>
                  type_cats_1 (a, fst <$> u, fst <$> e', fst <$> f', fst <$> c', fst <$> i') j' (k', l', g')))))
--------------------------------------------------------------------------------------------------------------------------------