--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas where
  import Cats_0
  import Cats_1
  import Data.Bifunctor
  import Data.Map
  import Datas_0
  import Datas_1
  import Datas_2
  import Naming
  import Standard
  import Tokenise
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
  comparison_kind :: Kind_1
  comparison_kind = Name_kind_1 "Ordering"
  comparison_type :: Type_1
  comparison_type = ntype "Ordering"
  constructors :: Map' Constructor
  constructors =
    Data.Map.fromList
      [
        ("EQ", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        ("GT", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        ("LT", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        ("Next", Constructor [] [] [ntype "Nat"] (ntype "Nat") [("Next", 1), ("Zero", 0)]),
        ("Zero", Constructor [] [] [] (ntype "Nat") [("Next", 1), ("Zero", 0)])]
  hkinds :: Map' Kind
  hkinds =
    Data.Map.fromList
      [("Kind arrow", Arrow_kind (Arrow_kind Star_kind)), ("Nat", Star_kind), ("Ordering", Star_kind), ("Star", Star_kind)]
  int_type :: Type_1
  int_type = ntype "Int"
  kinds :: Map' Polykind
  kinds =
    Data.Map.fromList
      [
        ("Arrow", Polykind (Just "k") [] (arrow_kind (Name_kind_1 "k") (arrow_kind (Name_kind_1 "k") (Name_kind_1 "Star")))),
        ("EQ", pkind comparison_kind),
        ("GT", pkind comparison_kind),
        ("Int", pkind star_kind),
        ("LT", pkind comparison_kind),
        ("Nat", pkind star_kind),
        ("Next", pkind (arrow_kind nat_kind nat_kind)),
        ("Ordering", pkind star_kind),
        ("Zero", pkind nat_kind)]
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "Nat"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  pconstrs :: Map' PConstructor
  pconstrs =
    (
      (\(Constructor _ a b c d) -> PConstructor ((\(Name_tpat e, _) -> e) <$> a) (prom_type <$> b) (prom_type c) d) <$>
      constructors)
  prom_algs :: Map' Prom_alg
  prom_algs =
    Data.Map.fromList
      [
        ("Nat", Prom_alg [] (Data.Map.fromList [("Next", [nat_kind]), ("Zero", [])])),
        ("Ordering", Prom_alg [] (Data.Map.fromList [("EQ", []), ("GT", []), ("LT", [])]))]
  promotables :: Map' Bool
  promotables = Data.Map.fromList  [("Arrow", False), ("Int", False), ("Nat", True), ("Ordering", True)]
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