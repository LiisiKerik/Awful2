--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Cats_1 (Cat_6 (..), repkinds, type_cats_1) where
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
  kvars :: [String] -> (Integer, Map' Kind_1, Set String) -> ((Integer, Map' Kind_1, Set String), [String])
  kvars a (b, c, d) =
    let
      f = b + fromIntegral (length a)
      e = show <$> [b .. f - 1]
    in
      ((f, Data.Map.union c (Data.Map.fromList (zip a (Name_kind_1 <$> e))), Data.Set.union d (Data.Set.fromList e)), e)
  repkinds :: Map' Kind_1 -> Kind_1 -> Kind_1
  repkinds a b =
    case b of
      Application_kind_1 c d -> Application_kind_1 (repkinds a c) (repkinds a d)
      Name_kind_1 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
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