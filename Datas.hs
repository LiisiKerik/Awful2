--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas where
  import Cats_0
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Datas_0
  import Datas_1
  import Datas_2
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Cat_6 = Cat_6 Location_0 (String, [String]) [String] (Expression_1, Expression_1) deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
  arrow_type :: Kind_1 -> Type_1 -> Type_1 -> Type_1
  arrow_type k a = Application_type_1 (Application_type_1 (Name_type_1 "Arrow" (Just k) []) a)
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
  kvars :: [String] -> (Integer, Map' Kind_1, Set String) -> ((Integer, Map' Kind_1, Set String), [String])
  kvars a (b, c, d) =
    let
      f = b + fromIntegral (length a)
      e = show <$> [b .. f - 1]
    in
      ((f, Data.Map.union c (Data.Map.fromList (zip a (Name_kind_1 <$> e))), Data.Set.union d (Data.Set.fromList e)), e)
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
  repkinds :: Map' Kind_1 -> Kind_1 -> Kind_1
  repkinds a b =
    case b of
      Application_kind_1 c d -> Application_kind_1 (repkinds a c) (repkinds a d)
      Name_kind_1 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in
      Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pat_1 <$> c)
  type_cat_1 ::
    (
      (String, Map' Kind, Map' PConstructor, Map' Polykind, Map' Prom_alg, Map' Cat_4) ->
      Cat_5 ->
      (Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2) ->
      Err ((Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2), Cat_6))
  type_cat_1 (a, o, t, u, a3, j) (Cat_5 b (p, d) n (e, f, g, h, i)) (k, l, m) =
    let
      x = Prelude.foldl Application_kind_1 (Name_kind_1 p) (Name_kind_1 <$> d)
      y = Data.Map.union o (Data.Map.fromList ((\y' -> (y', Star_kind)) <$> d))
    in
      (
        type_tpat' (Location_1 a) t e x u >>=
        \((v, e4), w, d2) ->
          (
            type_tpat' (Location_1 a) t f x w >>=
            \((a1, e5), t3, f9) ->
              let
                f7 = Data.Map.union d2 f9
              in
                (
                  type_data_br_1 (a, f7, a3) g m >>=
                  \(d3, k8) ->
                    (
                      type_data_br_2
                        (Location_1 a)
                        (arrow_type x e4 e5)
                        k8
                        t3
                        y
                        (k, l)
                        (d, [(v, x), (a1, x)])
                        "Arrow"
                        f7
                        (Data.Map.union j (Data.Map.fromList ((\k4 -> (k4, Cat_4 [] [])) <$> n))) >>=
                      \(k7, f0) -> Right ((k7, f0, d3), Cat_6 b (p, d) n (h, i))))))
  type_cats_1 ::
    (
      (String, Map' Kind, Map' PConstructor, Map' Polykind, Map' Prom_alg, Map' Cat_4) ->
      [Cat_5] ->
      (Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2) ->
      Err ((Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2), [Cat_6]))
  type_cats_1 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> type_cat_1 a d c >>= \(f, g) -> second ((:) g) <$> type_cats_1 a e f
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
  type_tpat ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      Patn' ->
      Kind_1 ->
      Map' Kind_1 ->
      Integer ->
      Set String ->
      [(Kind_1, Kind_1)] ->
      Integer ->
      Err ((TPat, Type_1), Map' Kind_1, Integer, Set String, [(Kind_1, Kind_1)], Integer))
  type_tpat k h b c d l n o d2 =
    case b of
      Application_patn' (Name g e) f ->
        und_err
        e
        h
        "constructor"
        (k g)
        (\(PConstructor i j m a) ->
          case a of
            [_] ->
              let
                ((p, q, r), a2) = kvars i (l, Data.Map.empty, n)
              in
                (
                  (\(s, t, u, v, w, a3) ->
                    (
                      (
                        Application_tpat e (fst <$> s),
                        Prelude.foldl Application_type_1 (Name_type_1 e Nothing (Name_kind_1 <$> a2)) (snd <$> s)),
                      t,
                      u,
                      v,
                      w,
                      a3)) <$>
                  type_tpats k h f (repkinds q <$> j) d p r ((c, repkinds q m) : o) (Name g e) d2)
            _ -> Left ("Constructor " ++ e ++ location (k g) ++ " is not a struct constructor."))
      Name_patn' e -> Right ((Name_tpat e, ntype e), Data.Map.insert e c d, l, n, o, d2)
  type_tpat' ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      (Location_0, Patn') ->
      Kind_1 ->
      Map' Polykind ->
      Err ((TPat, Type_1), Map' Polykind, Map' Kind_1))
  type_tpat' a b (l, c) d e =
    (
      type_tpat a b c d Data.Map.empty 0 Data.Set.empty [] 0 >>=
      \((f, u2), g, _, i, j, _) ->
        (\(h, m1, []) -> ((f, m1), Data.Map.union e (pkind <$> h), h)) <$> solve_type_eqs (a l) i j (g, u2, []))
  type_tpats ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      [Patn'] ->
      [Kind_1] ->
      Map' Kind_1 ->
      Integer ->
      Set String ->
      [(Kind_1, Kind_1)] ->
      Name ->
      Integer ->
      Err ([(TPat, Type_1)], Map' Kind_1, Integer, Set String, [(Kind_1, Kind_1)], Integer))
  type_tpats a b d e f g h i (Name x y) d3 =
    let
      z a' = Left ("Constructor " ++ y ++ location (a x) ++ " has been given too " ++ a' ++ " arguments.")
    in
      case d of 
        [] ->
          case e of
            [] -> Right ([], f, g, h, i, d3)
            _ -> z "few"
        j : k ->
          case e of
            [] -> z "many"
            m : n ->
              (
                type_tpat a b j m f g h i d3 >>=
                \(c, o, p, q, r, k2) ->
                  (\(s, t, u, v, w, n3) -> (c : s, t, u, v, w, n3)) <$> type_tpats a b k n o p q r (Name x y) k2)
--------------------------------------------------------------------------------------------------------------------------------