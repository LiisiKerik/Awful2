--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Datas_0
  import Datas_1
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Cat_5 =
    Cat_5
      Location_0
      (String, [String])
      [String]
      ((Location_0, Patn'), (Location_0, Patn'), Data_br_2, Expression_1, Expression_1)
        deriving Show
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
  repl_kind_vars :: String -> TPat -> [(TPat, Kind_1)] -> [(TPat, Kind_1)]
  repl_kind_vars a b = fmap (first (repl_kind_vars' a b))
  repl_kind_vars' :: String -> TPat -> TPat -> TPat
  repl_kind_vars' a b c =
    case c of
      Application_tpat e d -> Application_tpat e (repl_kind_vars' a b <$> d)
      Name_tpat d ->
        case a == d of
          False -> c
          True -> b
  sysrep' :: String -> Type_1 -> Type_1 -> Type_1
  sysrep' a b c =
    let
      f = sysrep' a b
    in
      case c of
        Application_type_1 d e -> Application_type_1 (f d) (f e)
        Name_type_1 d _ _ -> if d == a then b else c
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in
      Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pat_1 <$> c)
  type_branching_1 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      String ->
      Data_case_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Map' (Type_2, Status)) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Map' (Type_2, Status)))
  type_branching_1 a b c (Data_case_3 d e f) g h i (k', k) l m n n7 =
    type_data_br_2
      a
      (sysrep' c (Prelude.foldl (\o -> \(p, _) -> Application_type_1 o (ntype p)) (Name_type_1 d Nothing n) e) b)
      f
      (Prelude.foldl (\o -> \(p, q) -> Data.Map.insert p (pkind q) o) g e)
      h
      i
      (k', repl_kind_vars c (Application_tpat d ((\(w, _) -> Name_tpat w) <$> e)) k)
      l
      (Prelude.foldl (\o -> \(p, q) -> Data.Map.insert p q o) m e)
      n7
  type_branchings_1 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      String ->
      [Data_case_3] ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Map' (Type_2, Status)) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Map' (Type_2, Status)))
  type_branchings_1 a b c e f g h i j k n n7 =
    case e of
      [] -> Right h
      l : m -> type_branching_1 a b c l f g h i j k n n7 >>= \d -> type_branchings_1 a b c m f g d i j k n n7
  type_cat_0 :: (Location_0 -> Location_1, Map' Kind) -> Cat_3 -> Map' (Cat_4, Status) -> Err (Map' (Cat_4, Status), Cat_5)
  type_cat_0 (a, o) (Cat_3 b (Name c p, d) n e) j =
    und_err
      p
      o
      "kind"
      (a c)
      (\q ->
        case type_cat_help q d of
          LT -> Left ("Kind constructor " ++ p ++ location (a c) ++ " has been given too few arguments.")
          EQ ->
            (
              type_cat_constrs (a, Data.Set.fromList d) (fst <$> j, n) >>=
              \(_, s) -> Right (ins_new p (Cat_4 d s) j, Cat_5 b (p, d) s e))
          GT -> Left ("Kind constructor " ++ p ++ location (a c) ++ " has been given too many arguments."))
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
  type_cat_constrs :: (Location_0 -> Location_1, Set String) -> (Map' Cat_4, [Name]) -> Err (Map' Cat_4, [String])
  type_cat_constrs (g, a) (b, c) =
    case c of
      [] -> Right (b, [])
      Name d e : f ->
        case Data.Set.member e a of
          False -> Left ("Undefined kind variable " ++ e ++ location' (g d))
          True ->
            case Data.Map.lookup e b of
              Nothing -> second ((:) e) <$> type_cat_constrs (g, a) (Data.Map.insert e (Cat_4 [] []) b, f)
              Just _ -> Left ("Duplicate category constraint for " ++ e ++ location' (g d))
  type_cat_help :: Kind -> [String] -> Ordering
  type_cat_help a b =
    case (a, b) of
      (Star_kind, []) -> EQ
      (Star_kind, _) -> GT
      (Arrow_kind _, []) -> LT
      (Arrow_kind c, _ : d) -> type_cat_help c d
  type_cats_0 :: (Location_0 -> Location_1, Map' Kind) -> [Cat_3] -> Map' (Cat_4, Status) -> Err (Map' (Cat_4, Status), [Cat_5])
  type_cats_0 g a b =
    case a of
      [] -> Right (b, [])
      c : d -> type_cat_0 g c b >>= \(e, f) -> second ((:) f) <$> type_cats_0 g d e
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
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_3 ->
      Map' Polykind ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Constructor, Status), Map' (Type_2, Status)) ->
      Err (Map' (Constructor, Status), Map' (Type_2, Status)))
  type_data_2 a (Data_3 b c d) e f g4 g =
    type_data_br_2
      a
      (Prelude.foldl (\n -> \n' -> Application_type_1 n (ntype n')) (ntype b) (fst <$> c))
      d
      (type_kinds c e)
      f
      g
      ([], first Name_tpat <$> c)
      b
      (Prelude.foldl (\m -> \(x, y) -> Data.Map.insert x y m) Data.Map.empty c)
      g4
  type_data_br_2 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      Data_br_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Map' (Type_2, Status)) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Map' (Type_2, Status)))
  type_data_br_2 a b c d e (f, g) (x, l) o w1 f7 =
    let
      m i = Prelude.foldl (\j -> \(k, n) -> ins_new k (Type_2 Nothing x [] l Nothing [] n) j) g i
    in
      case c of
        Algebraic_data_3 i ->
          (
            (\j ->
              (
                Prelude.foldl
                  (\k -> \(Form_2 n p) ->
                    ins_new n (Constructor x l p b ((\(Form_2 h y) -> (h, fromIntegral (length y))) <$> j)) k)
                  f
                  j,
                m ((\(Form_2 k n) -> (k, Prelude.foldr function_type b n)) <$> j))) <$>
            type_forms a i d e f7)
        Branching_data_3 j h k -> type_branchings_1 a b j k (Data.Map.delete j d) e (f, g) (x, l) o (Data.Map.delete j w1) h f7
        Struct_data_3 i j ->
          (
            (\k ->
              (
                ins_new i (Constructor x l (snd <$> k) b [(i, fromIntegral (length j))]) f,
                m ((i, Prelude.foldr function_type b (snd <$> k)) : (second (function_type b) <$> k)))) <$>
            type_fields a j d e f7)
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
  type_datas_2 ::
    (
      (Location_0 -> Location_1) ->
      [Data_3] ->
      Map' Polykind ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Constructor, Status), Map' (Type_2, Status)) ->
      Err (Map' (Constructor, Status), Map' (Type_2, Status)))
  type_datas_2 f a b y c' c =
    case a of
      [] -> Right c
      d : e -> type_data_2 f d b y c' c >>= type_datas_2 f e b y c'
  type_kind :: (String, Kind_1) -> Map' Polykind -> Map' Polykind
  type_kind (a, b) = Data.Map.insert a (pkind b)
  type_kinds :: [(String, Kind_1)] -> Map' Polykind -> Map' Polykind
  type_kinds a b =
    case a of
      [] -> b
      c : d -> type_kinds d (type_kind c b)
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