--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Defs where
  import Classes
  import Control.Monad
  import Data.Bifunctor
  import Data.Foldable
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Datas
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_3 = Blank_alg_pat_3 | Char_alg_pat_3 Char | Int_alg_pat_3 Integer | Struct_alg_pat_3 String [Alg_pat_3]
    deriving Show
  data Def_4 =
    Basic_def_4 Location_0 String KT2 [Constraint_1] Type_1 Expression_1 [String] |
    Instance_4
      Location_0
      String
      (Maybe String)
      String
      String
      [(String, Kind_1)]
      Integer
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)]
      Type_1
      [Constraint_1]
      [String]
      [[String]]
        deriving Show
  data Eqtn = Kind_eq Kind_1 Kind_1 | Type_eq Type_1 Type_1 deriving Show
  data Eqtns = Eqtns (Set String, Set String) [Eqtn] [Kind_1] [(String, (Name, Type_1))] deriving Show
  data KT2 = KT2 [String] [String] [(String, Kind_1)] deriving Show
  data Inst = Inst [Kind_1] [[String]] deriving Show
  data Pattern_5 =
    Blank_pattern_5 |
    Char_blank_pattern_5 (Set Char) |
    Char_pattern_5 Char |
    Int_blank_pattern_5 (Set Integer) |
    Int_pattern_5 Integer |
    Struct_pattern_5 String [Pattern_5]
      deriving Show
  data Typedexpr =
    Application_texpr Typedexpr Typedexpr |
    Char_texpr Char |
    Function_texpr Pat_1 Typedexpr |
    Int_texpr Integer |
    Match_texpr Typedexpr [(Alg_pat_2, Typedexpr)] |
    Name_texpr_0 String String Type_1 |
    Name_texpr_1 String [(String, Type_1)]
      deriving Show
  addargs :: Map' ([String], Map' [(String, Nat)]) -> Typedexpr -> Expression_2
  addargs b c =
    let
      h = addargs b
    in
      case c of
        Application_texpr d e -> Application_expression_2 (h d) (h e)
        Char_texpr d -> Char_expression_2 d
        Function_texpr d e -> Function_expression_2 d (h e)
        Int_texpr d -> Int_expression_2 d
        Match_texpr d e -> Match_expression_2 (h d) (second h <$> e)
        Name_texpr_0 d e f ->
          let
            (g, i) = typestring f []
          in
            addargs_2
              b
              (
                second (getarg i) <$>
                case Data.Map.lookup g (snd (b ! e)) of
                  Just k -> k
                  Nothing -> [])
              (Name_expression_2 (d ++ " " ++ g))
        Name_texpr_1 d e -> addargs_2 b e (Name_expression_2 d)
  addargs_1 :: Map' ([String], Map' [(String, Nat)]) -> String -> String -> [Type_1] -> Expression_2 -> Expression_2
  addargs_1 c d e f g =
    let
      (h, i) = c ! d
    in
      Prelude.foldl
        (\j -> \k ->
          Application_expression_2
            j
            (addargs_2
              c
              (
                second (getarg f) <$>
                case Data.Map.lookup e i of
                  Just l -> l
                  Nothing -> [])
              (Name_expression_2 (k ++ " " ++ e))))
        g
        h
  addargs_2 :: Map' ([String], Map' [(String, Nat)]) -> [(String, Type_1)] -> Expression_2 -> Expression_2
  addargs_2 b d e =
    case d of
      [] -> e
      (f, g) : h ->
        let
          (c, i) = typestring g []
        in
          addargs_2 b h (addargs_1 b f c i e)
  compose_patterns :: (Pattern_5, Bool) -> ([Pattern_5], Bool) -> ([Pattern_5], Bool)
  compose_patterns (x, y) (z, w) = (x : z, y && w)
  constr_check :: Map' Class_4 -> [String] -> [[String]] -> [[String]] -> Maybe String
  constr_check m t x y =
    case t of
      [] -> Nothing
      s : t' ->
        case x of
          [] -> undefined
          a : x' ->
            case y of
              [] -> undefined
              b : y' ->
                case constr_check' m s a b of
                  Just c -> Just c
                  Nothing -> constr_check m t' x' y'
  constr_check' :: Map' Class_4 -> String -> [String] -> [String] -> Maybe String
  constr_check' n t x y =
    case x of 
      [] -> Nothing
      a : x' ->
        case constr_check'' n t a y of
          Left m -> Just m
          Right y' -> constr_check' n t x' y'
  constr_check'' :: Map' Class_4 -> String -> String -> [String] -> Err [String]
  constr_check'' m t c x =
    case x of
      [] -> Left (c ++ " " ++ t)
      a : x' -> if constr_check_3 m c a then Right x' else (:) a <$> constr_check'' m t c x'
  constr_check_3 :: Map' Class_4 -> String -> String -> Bool
  constr_check_3 m x y =
    case x == y of
      False ->
        let
          Class_4 _ _ _ a _ = m ! y
        in
          case a of
            Just (y', _) -> constr_check_3 m x y'
            Nothing -> False
      True -> True
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor) ->
      ((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2) ->
      Alg_pat_1 ->
      Type_1 ->
      Err (((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2), (Alg_pat_2, Alg_pat_3)))
  get_pattern_type a (b', b) (d, e, f, n) g h =
    case g of
      Application_alg_pat_1 (Name o i) j ->
        und_err
          i
          b
          "constructor"
          (a o)
          (\(Constructor k u1 x m _) ->
            let
              (q, r, s) = typevars b' (k, u1) (d, Data.Map.empty, e)
            in
              (
                second (bimap (Application_alg_pat_2 i) (Struct_alg_pat_3 i)) <$>
                get_pattern_types a (b', b) (q, s, Type_eq h (repl' r m) : f, n) j (repl' r <$> x) (Name o i)))
      Blank_alg_pat_1 -> Right ((d, e, f, n), (Blank_alg_pat_2, Blank_alg_pat_3))
      Char_alg_pat_1 i -> Right ((d, e, Type_eq h char_type : f, n), (Char_alg_pat_2 i, Char_alg_pat_3 i))
      Int_alg_pat_1 i -> Right ((d, e, Type_eq h int_type : f, n), (Int_alg_pat_2 i, Int_alg_pat_3 i))
      Name_alg_pat_1 i ->
        Right ((d, e, f, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] h) n), (Name_alg_pat_2 i, Blank_alg_pat_3))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor) ->
      ((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      Err (((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2), ([Alg_pat_2], [Alg_pat_3])))
  get_pattern_types a b d e f (Name m n) =
    case (e, f) of
      ([], []) -> Right (d, ([], []))
      ([], _) -> Left ("Constructor " ++ n ++ location (a m) ++ " has been given too few arguments.")
      (_, []) -> Left ("Constructor " ++ n ++ location (a m) ++ " has been given too many arguments.")
      (g : h, i : j) ->
        get_pattern_type a b d g i >>= \(k, (l, t)) -> second (bimap ((:) l) ((:) t)) <$> get_pattern_types a b k h j (Name m n)
  getarg :: [t] -> Nat -> t
  getarg a b =
    case a of
      [] -> undefined
      c : d ->
        case b of
          Nxt e -> getarg d e
          Zr -> c
  instances :: Map' (Map' Inst)
  instances =
    Data.Map.fromList
      [
        ("Ord", Data.Map.fromList [("Char", Inst [] []), ("Int", Inst [] [])]),
        ("Ring", Data.Map.fromList [("Int", Inst [] [])]),
        ("Writeable", Data.Map.fromList [("Int", Inst [] [])])]
  kindvar :: String -> (Integer, Set String) -> (Integer, Set String)
  kindvar _ (b, c) = (b + 1, Data.Set.insert (show b) c)
  kindvars :: [String] -> (Integer, Set String) -> (Integer, Set String)
  kindvars a b =
    case a of
      [] -> b
      d : e -> kindvars e (kindvar d b)
  krep_eq :: (Kind_1 -> Kind_1) -> Eqtn -> Eqtn
  krep_eq a b =
    case b of
      Kind_eq c d -> Kind_eq (a c) (a d)
      Type_eq c d -> Type_eq (type_rep a c) (type_rep a d)
  new_typevar :: (Integer, Set String) -> Kind_1 -> ((Integer, Set String), Type_1)
  new_typevar (a, b) d =
    let
      c = show a
    in
      ((a + 1, Data.Set.insert c b), ntype c)
  occ_check :: String -> Type_1 -> Bool
  occ_check a b =
    case b of
      Application_type_1 c d -> occ_check a c || occ_check a d
      Name_type_1 c _ _ -> a == c
  occ_k :: String -> Kind_1 -> Bool
  occ_k a b =
    case b of
      Application_kind_1 c d -> occ_k a c || occ_k a d
      Name_kind_1 c -> a == c
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  pats :: (Location_0 -> Location_1) -> Map' [(String, Integer)] -> [(Location_0, [Alg_pat_3])] -> Err ()
  pats a b = traverse_ (\(d, e) -> patterns b (a d, e))
  pattern :: Map' [(String, Integer)] -> [(Pattern_5, Bool)] -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  pattern context x y =
    case x of
      [] -> ([], False)
      z : a ->
        let
          (b, c) = pattern' context z y
        in
          bimap ((++) b) ((||) c) (pattern context a y)
  pattern' :: Map' [(String, Integer)] -> (Pattern_5, Bool) -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  pattern' context (x, y) z =
    case y of
      False -> split_pattern context x z
      True -> ([(x, True)], False)
  patterns :: Map' [(String, Integer)] -> (Location_1, [Alg_pat_3]) -> Err ()
  patterns context (l, x) =
    (
      patterns' context [(Blank_pattern_5, False)] (l, x) >>=
      \y ->
        case all snd y of
          False -> Left ("Incomplete match" ++ location' l)
          True -> Right ())
  patterns' :: Map' [(String, Integer)] -> [(Pattern_5, Bool)] -> (Location_1, [Alg_pat_3]) -> Err [(Pattern_5, Bool)]
  patterns' context x (l, y) =
    case y of
      [] -> Right x
      z : a ->
        let
          (b, c) = pattern context x z
        in
          case c of
            False -> Left ("Unnecessary patterns" ++ location' l)
            True -> patterns' context b (l, a)
  primitive_pattern_0 :: (t -> Pattern_5, Set t -> Pattern_5) -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_0 (f, g) x = ([(f x, True), (g (Data.Set.singleton x), False)], True)
  primitive_pattern_1 :: Eq t => (t -> Pattern_5) -> t -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_1 f x y =
    let
      z = y == x
    in
      ([(f x, z)], z)
  primitive_pattern_2 :: Ord t => (t -> Pattern_5, Set t -> Pattern_5) -> Set t -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_2 (f, g) x y =
    case Data.Set.member y x of
      False -> ([(f y, True), (g (Data.Set.insert y x), False)], True)
      True -> ([(g x, False)], False)
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  rep_eq :: (Type_1 -> Type_1) -> Eqtn -> Eqtn
  rep_eq a b =
    case b of
      Kind_eq _ _ -> b
      Type_eq c d -> Type_eq (a c) (a d)
  repkinds_method :: Map' Kind_1 -> Method_4 -> Method_4
  repkinds_method a (Method_4 b c d e) = Method_4 b (second (repkinds a) <$> c) d (repkinds_type a e)
  repkinds_type :: Map' Kind_1 -> Type_1 -> Type_1
  repkinds_type a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repkinds_type a c) (repkinds_type a d)
      Name_type_1 c e d -> Name_type_1 c (repkinds a <$> e) (repkinds a <$> d)
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c _ _ ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
  skinds :: Kind_1 -> Kind_1 -> Map' Kind_1
  skinds a b =
    case (a, b) of
      (Application_kind_1 c d, Application_kind_1 e f) -> Data.Map.union (skinds c e) (skinds d f)
      (Name_kind_1 c, _) -> Data.Map.singleton c b
      _ -> undefined
  slv :: Map' (Map' Inst) -> [(String, (Name, Type_1))] -> (Name -> String -> String -> String) -> Err ()
  slv a b h =
    case b of
      [] -> Right ()
      (c, (y, d)) : e ->
        let
          (f, g) = typestring d []
          i = Left (h y c f)
        in
          case Data.Map.lookup c a of
            Just x ->
              case Data.Map.lookup f x of
                Just (Inst m j) -> slv_constrs a e h g j y
                Nothing -> i
            Nothing -> i
  slv_constrs ::
    (
      Map' (Map' Inst) ->
      [(String, (Name, Type_1))] ->
      (Name -> String -> String -> String) ->
      [Type_1] ->
      [[String]] ->
      Name ->
      Err ())
  slv_constrs a b c d e y =
    case d of
      [] ->
        case e of
          [] -> slv a b c
          _ -> undefined
      f : g ->
        case e of
          [] -> undefined
          h : i -> slv_constrs a (((\j -> (j, (y, f))) <$> h) ++ b) c g i y
  solvek' ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      Eqtn ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvek' m3 h b c d eq (y, x, m, (w, a)) =
    case Data.Set.member b w of
      False -> h
      True -> solvek_rep m3 h b c d (y, x, m, (w, a))
  solvek_rep ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvek_rep m3 a c d e (y, x, f, (w, k)) =
    let
      m = kindrep c d
    in
      case occ_k c d of
        False -> solvesys m3 (krep_eq m <$> e) (m <$> y, second (second (type_rep m)) <$> x, f, (Data.Set.delete c w, k))
        True -> a
  solvesys ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys m b (a2, a', t, (w, u)) =
    case b of
      [] -> Right (a2, a', t, (w, u))
      eq : g ->
        case eq of
          Kind_eq c d ->
            let
              e3 = m "Kind" eq
            in
              case (c, d) of
                (Application_kind_1 e f, Application_kind_1 h i) ->
                  solvesys m (Kind_eq e h : Kind_eq f i : g) (a2, a', t, (w, u))
                (Name_kind_1 e, Name_kind_1 f) ->
                  case e == f of
                    False ->
                      case (Data.Set.member e w, Data.Set.member f w) of
                        (False, False) -> e3
                        (True, _) -> solvek_rep m e3 e d g (a2, a', t, (w, u))
                        (_, True) -> solvek_rep m e3 f c g (a2, a', t, (w, u))
                    True -> solvesys m g (a2, a', t, (w, u))
                (Name_kind_1 e, _) -> solvek' m e3 e d g eq (a2, a', t, (w, u))
                (_, Name_kind_1 e) -> solvek' m e3 e c g eq (a2, a', t, (w, u))
          Type_eq c d ->
            let
              e3 = m "Type" eq
            in
              case (c, d) of
                (Application_type_1 e f, Application_type_1 h i) ->
                  solvesys m (Type_eq e h : Type_eq f i : g) (a2, a', t, (w, u))
                (Name_type_1 e e0 e1, Name_type_1 f f0 f1) ->
                  case e == f of
                    False ->
                      case (Data.Set.member e u, Data.Set.member f u) of
                        (False, False) -> e3
                        (True, _) -> solvesys_rep m e3 e d g (a2, a', t, (w, u))
                        (_, True) -> solvesys_rep m e3 f c g (a2, a', t, (w, u))
                    True -> solvesys m (maybeToList (Kind_eq <$> e0 <*> f0) ++ zipWith Kind_eq e1 f1 ++ g) (a2, a', t, (w, u))
                (Name_type_1 e _ _, _) -> solvesys' m e3 e d g (a2, a', t, (w, u))
                (_, Name_type_1 e _ _) -> solvesys' m e3 e c g (a2, a', t, (w, u))
  solvesys' ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys' m3 h b c d (y, x, m, (w, a)) =
    case Data.Set.member b a of
      False -> h
      True -> solvesys_rep m3 h b c d (y, x, m, (w, a))
  solvesys_rep ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys_rep m3 a c d e (y, x, f, (w, k)) =
    let
      m = sysrep' c d
    in
      case occ_check c d of
        False -> solvesys m3 (rep_eq m <$> e) (y, second (second m) <$> x, sysrep2 c d f, (w, Data.Set.delete c k))
        True -> a
  split_pattern :: Map' [(String, Integer)] -> Pattern_5 -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  split_pattern context x y =
    case (x, y) of
      (Blank_pattern_5, Char_alg_pat_3 z) -> primitive_pattern_0 (Char_pattern_5, Char_blank_pattern_5) z
      (Blank_pattern_5, Int_alg_pat_3 z) -> primitive_pattern_0 (Int_pattern_5, Int_blank_pattern_5) z
      (Blank_pattern_5, Struct_alg_pat_3 z a) ->
        (
          join
            (
              (\(b, c) ->
                case b == z of
                  False -> [(Struct_pattern_5 b (replicate (fromIntegral c) Blank_pattern_5), False)]
                  True -> fst (struct_pattern context z ((,) Blank_pattern_5 <$> a))) <$>
              context ! z),
          True)
      (Char_blank_pattern_5 z, Char_alg_pat_3 a) -> primitive_pattern_2 (Char_pattern_5, Char_blank_pattern_5) z a
      (Char_pattern_5 z, Char_alg_pat_3 a) -> primitive_pattern_1 Char_pattern_5 z a
      (Int_blank_pattern_5 z, Int_alg_pat_3 a) -> primitive_pattern_2 (Int_pattern_5, Int_blank_pattern_5) z a
      (Int_pattern_5 z, Int_alg_pat_3 a) -> primitive_pattern_1 Int_pattern_5 z a
      (Struct_pattern_5 z a, Struct_alg_pat_3 b c) ->
        case b == z of
          False -> ([(x, False)], False)
          True -> struct_pattern context z (zip a c)
      (_, Blank_alg_pat_3) -> ([(x, True)], True)
      _ -> undefined
  struct_pattern :: Map' [(String, Integer)] -> String -> [(Pattern_5, Alg_pat_3)] -> ([(Pattern_5, Bool)], Bool)
  struct_pattern context x y =
    first
      (fmap (first (Struct_pattern_5 x)))
      (Prelude.foldr
        (\(z, a) -> \(b, c) -> (compose_patterns <$> z <*> b, a && c))
        ([([], True)], True)
        (uncurry (split_pattern context) <$> y))
  sysrep2 :: String -> Type_1 -> Typedexpr -> Typedexpr
  sysrep2 a b c =
    let
      f = sysrep2 a b
    in
      case c of
        Application_texpr d e -> Application_texpr (f d) (f e)
        Function_texpr d e -> Function_texpr d (f e)
        Match_texpr d e -> Match_texpr (f d) (second f <$> e)
        Name_texpr_0 d g e -> Name_texpr_0 d g (sysrep' a b e)
        Name_texpr_1 d e -> Name_texpr_1 d (second (sysrep' a b) <$> e)
        _ -> c
  type_case ::
    (
      (Map' PConstructor, Map' Constructor) ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      (Alg_pat_1, Expression_1) ->
      Type_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ((Alg_pat_2, Typedexpr), Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a c d (Eqtns e p y' q) f (g, h) i j k =
    (
      get_pattern_type c a (d, e, p, f) g i >>=
      \((m, n, s, t), (o, y)) -> (\(u, v, w, r0) -> ((o, u), v, w, r0, y)) <$> type_expression a c m (Eqtns n s y' q) t h j k)
  type_cases ::
    (
      (Map' PConstructor, Map' Constructor) ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      [(Alg_pat_1, Expression_1)] ->
      Type_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ([(Alg_pat_2, Typedexpr)], Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])], [Alg_pat_3]))
  type_cases b c d e f g n h i =
    case g of
      [] -> Right ([], e, d, [], [])
      l : m ->
        (
          type_case b c d e f l n h i >>=
          \(o, p, q, r0, r') -> (\(r, s, t, r1, w) -> (o : r, s, t, r0 ++ r1, r' : w)) <$> type_cases b c q p f m n h i)
  type_cat_2 ::
    (
      (
        Location_0 -> Location_1,
        Map' Kind,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' PConstructor,
        Map' Constructor,
        Map' Type_2) ->
      Cat_6 ->
      (Map' Expression_2) ->
      Err (Map' Expression_2))
  type_cat_2 (a, o, u, m', z', j, f7, k7, l) (Cat_6 b (p, d) n (h, i)) m =
    let
      x a1 a2 = arrow_type (Prelude.foldl Application_kind_1 (Name_kind_1 p) (Name_kind_1 <$> d)) (ntype a1) (ntype a2)
      y = Data.Map.union o (Data.Map.fromList ((\y' -> (y', Star_kind)) <$> d))
      s' m7 fj u1 =
        type_expr
          (m7 ++ " " ++ p ++ location' (a b))
          fj
          a
          (f7, k7, l)
          u1
          z'
          0
          m'
          (u, y)
          (Prelude.foldl (\m3 -> \t5 -> Data.Map.insert t5 (Cat_4 [] []) m3) j n)
          b
    in
      (
        (\j7 -> \j8 -> Data.Map.union m (Data.Map.fromList [("compose " ++ p, j7), ("id " ++ p, j8)])) <$>
        s' "compose" (function_type (x "t" "u") (function_type (x "v" "t") (x "v" "u"))) h <*>
        s' "id" (x "t" "t") i)
  type_cats_2 ::
    (
      (
        Location_0 -> Location_1,
        Map' Kind,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' PConstructor,
        Map' Constructor,
        Map' Type_2) ->
      [Cat_6] ->
      (Map' Expression_2) ->
      Err (Map' Expression_2))
  type_cats_2 a b c =
    case b of
      [] -> Right c
      d : e -> type_cat_2 a d c >>= type_cats_2 a e
  type_class_args ::
    (
      Kind_1 ->
      [Pattern_0] ->
      Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat) ->
      Kind_1 ->
      Integer ->
      Type_1 ->
      Map' Kind_1 ->
      Map' Nat ->
      Nat ->
      Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat))
  type_class_args a b e g c x c0 c' n =
    case b of
      [] -> if a == g then Right ([], c, x, c0, c') else e
      h : d ->
        case a of
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") l) f ->
            let
              i j k =
                (
                  (\(t, u, v, t', t2) -> ((j, l) : t, u, v, t', t2)) <$>
                  type_class_args
                    f
                    d
                    e
                    g
                    k
                    (Application_type_1 x (ntype j))
                    (Data.Map.insert j l c0)
                    (Data.Map.insert j n c')
                    (Nxt n))
            in
              case h of
                Blank_pattern_0 -> i (show c) (c + 1)
                Name_pattern_0 j -> i j c
          _ -> e
  type_cls_0 ::
    String ->
    [Method_4] ->
    Type_1 ->
    [(Name, Expression_1)] ->
    (Location_0 -> Location_1) ->
    String ->
    Location_0 ->
    Err [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)]
  type_cls_0 a b c d l m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          (Method_4 e _ _ _) : _ -> Left ("Missing definition " ++ e ++ " in " ++ m ++ " " ++ a ++ location' (l n))
      (p' @ (Name h i), j) : k ->
        let
          o p = Left ("Definition " ++ i ++ location (l h) ++ " is not a method of class " ++ m ++ p)
        in
          case b of
            [] -> o "."
            (Method_4 e s y f) : g ->
              if i == e
                then (:) (p', j, s, y, f) <$> type_cls_0 a g c k l m n
                else o " or the definitions are in a wrong order."
  type_constraint_1 :: Constraint_1 -> Map' (Map' Inst) -> Map' Class_4 -> Map' (Map' Inst)
  type_constraint_1 (Constraint_1 c m e) a b =
    let
      d =
        Data.Map.insert
          c
          (Data.Map.insert
            e
            (Inst m [])
            (case Data.Map.lookup c a of
              Just l -> l
              Nothing -> Data.Map.empty))
          a
      Class_4 _ _ _ f _ = b ! c
    in
      case f of
        Just (g, _) -> type_constraint_1 (Constraint_1 g [] e) d b
        Nothing -> d
  type_constraints_1 :: [Constraint_1] -> Map' (Map' Inst) -> Map' Class_4 -> Map' (Map' Inst)
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_def_1 ::
    (
      String ->
      Map' Kind ->
      Def_3 ->
      Map' Polykind ->
      Map' (Type_2, Status) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err (Def_4, Map' (Type_2, Status), (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)]))))
  type_def_1 l x a b c k k2 w4 (t', u3) =
    case a of
      Basic_def_3 f d e e' g i ->
        (
          type_kt_1 (Location_1 l) ((x, b, Data.Map.empty, w4), e) >>=
          \((x2, j, j', u9), KT2 e0 m4 y) ->
            (
              type_constraints_0 Data.Map.empty e' (x2, k2, j', (\_ -> Zr) <$> j', u9) l >>=
                \(o1, o2, _) ->
                (
                  (\h ->
                    (
                      Basic_def_4 f d (KT2 e0 m4 y) o1 h i o2,
                      ins_new d (Type_2 Nothing e0 [] (first Name_tpat <$> y) Nothing o1 h) c,
                      (t', u3))) <$>
                  type_typ (Location_1 l) j x2 star_kind g u9)))
      Instance_3 d t8 a9 (Name e m) k3 (Name f n, u4, w2, k') o' g ->
        und_err
          m
          k
          "class"
          (Location_1 l e)
          (\(Class_4 d2 k9 (o, p) w0 q) ->
            let
              x4 = Data.Map.union x (Data.Map.fromList ((\t7 -> (t7, Star_kind)) <$> t8))
            in
              (
                type_cat_constrs (Location_1 l, Data.Set.fromList t8) (w4, a9) >>=
                \(w5, _) ->
                  (
                    ziphelp (Location_1 l) x4 m e Data.Map.empty d2 k3 >>=
                    \(g2, g9) ->
                      (
                        check_cats (Location_1 l d) w5 g9 k9 *>
                        und_err
                          n
                          b
                          "type"
                          (Location_1 l f)
                          (\(Polykind t1 r s) ->
                            (
                              ziph (Location_1 l) x4 n f (t1, r) (u4, w2) >>=
                              \((t6, e4), w3) ->
                                (
                                  type_class_args
                                    (repkinds w3 s)
                                    k'
                                    (kind_err (Location_1 l f))
                                    (repkinds g9 p)
                                    0
                                    (Name_type_1 n t6 e4)
                                    Data.Map.empty
                                    Data.Map.empty
                                    Zr >>=
                                  \(q', p', s', t0, t7) ->
                                    (
                                      type_constraints_0 Data.Map.empty o' (x4, k2, t0, t7, w5) l >>=
                                      \(o1, o2, o3) ->
                                        let
                                          r' =
                                            (
                                              (\(x', _) ->
                                                (
                                                  (\(Constraint_1 y' _ _) -> y') <$>
                                                  Data.List.filter (\(Constraint_1 _ _ y') -> y' == x') o1)) <$>
                                              q')
                                        in
                                          (
                                            (\w ->
                                              (
                                                Instance_4 d m (fst <$> w0) o n q' p' w s' o1 o2 r',
                                                c,
                                                (
                                                  case Data.Map.lookup m t' of
                                                    Nothing -> Data.Map.insert m (Data.Map.singleton n (Inst g2 r', New)) t'
                                                    Just _ -> adjust (ins_new n (Inst g2 r')) m t',
                                                  adjust (second (Data.Map.insert n o3)) m u3))) <$>
                                            type_cls_0 n (repkinds_method g9 <$> q) s' g (Location_1 l) m d)))))))))
  type_def_2 ::
    (
      (Location_0 -> Location_1) ->
      Def_4 ->
      (Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Map' Expression_2 ->
      Map' (Map' Inst) ->
      Map' Polykind ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      Map' Kind ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_def_2 j a d c m n t' u0 v2 f1 =
    case a of
      Basic_def_4 r e (KT2 b0 g3 b) x h i y' ->
        (
          (\t -> Data.Map.insert e (Prelude.foldr (\x' -> Function_expression_2 (Name_pat_1 x')) t y') c) <$>
          type_expr
            ("definition " ++ e ++ location' (j r))
            h
            j
            d
            i
            (type_constraints_1 x m u0)
            0
            t'
            (
              Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z (pkind w) y) n b,
              Prelude.foldl (\m0 -> \m1 -> Data.Map.insert m1 Star_kind m0) v2 b0)
            (Prelude.foldl (\y -> \z -> Data.Map.insert z (Cat_4 [] []) y) f1 g3)
            r)
      Instance_4 l' e' w0 w e e0 e1 f f' g' c2 r' ->
        let
          f4 = Prelude.foldl (\x -> \(y, g) -> Data.Map.insert y (pkind g) x) n e0
          r =
            type_exprs
              (\(Name x g) -> "definition " ++ g ++ " " ++ e ++ location' (j x))
              j
              d
              (type_constraints_1 g' m u0)
              f
              c
              e
              (sysrep' w f')
              e1
              (\g -> Prelude.foldr (\b -> Function_expression_2 (Name_pat_1 b)) g c2)
              t'
              u0
              (f4, v2)
              f1
          s' w1 = Left (e' ++ " " ++ e ++ " at" ++ location (j l') ++ " is an illegal instance because " ++ w1 ++ ".")
        in
          case w0 of
            Just q ->
              let
                s = s' (e' ++ " assumes " ++ q)
              in
                case Data.Map.lookup q m of
                  Just t ->
                    case Data.Map.lookup e t of
                      Just (Inst u4 u) ->
                        (
                          check_cats_2 (j l') f1 u4 *>
                          case constr_check u0 (fst <$> e0) u r' of
                            Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                            Nothing -> r)
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
  type_defs ::
    (
      String ->
      (Map' Kind, Map' Polykind, Map' Class_4, Map' Class_5, Map' Cat_4, Map' Constructor, Map' PConstructor) ->
      ([Def_3], [Cat_6]) ->
      (Types, Map' (Map' Inst), Map' Expression_2, Map' ([String], Map' [(String, Nat)])) ->
      Err (Map' Type_2, Map' (Map' Inst), Map' Expression_2, Map' ([String], Map' [(String, Nat)])))
  type_defs a (u, f', q', u', i', m', f2) (f, p') (r', n, o', s') =
    (
      type_defs_1 a u f f' r' q' u' i' (old' n, s') >>=
      \(v', w', (x', y')) ->
        (
          type_cats_2 (Location_1 a, u, f', y', fmap fst <$> x', i', f2, m', fst <$> w') p' o' >>=
          \a0 ->
            (
              (\b0 -> (rem_old w', rem_old' x', b0, y')) <$>
              type_defs_2 (Location_1 a) v' (f2, m', fst <$> w') a0 (fmap fst <$> x') f' y' q' u i')))
  type_defs_1 ::
    (
      String ->
      Map' Kind ->
      [Def_3] ->
      Map' Polykind ->
      Types ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err ([Def_4], Types, (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)]))))
  type_defs_1 h x a b c y y0 v u =
    case a of
      [] -> Right ([], c, u)
      d : e ->
        type_def_1 h x d b c y y0 v u >>= \(f, g, u') -> (\(k, l, m) -> (f : k, l, m)) <$> type_defs_1 h x e b g y y0 v u'
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' PConstructor, Map' Constructor, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' Inst) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Kind ->
    Map' Cat_4 ->
    Err (Map' Expression_2)
  type_defs_2 f a b c g i j u v h' =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i j u v h' >>= \h -> type_defs_2 f e b h g i j u v h'
  type_expr ::
    (
      String ->
      Type_1 ->
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Expression_1 ->
      Map' (Map' Inst) ->
      Integer ->
      Map' ([String], Map' [(String, Nat)]) ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Location_0 ->
      Err Expression_2)
  type_expr k h a (c4, c, e) f m w w' b a3 l3 =
    let
      n = " in " ++ k
    in
      (
        type_expression (c4, c) a (0, w) (Eqtns (Data.Set.empty, Data.Set.empty) [] [] []) e f h b >>=
        \(g, (Eqtns i j q8 x), _, x3) ->
          (
            solvesys
              (\w2 -> \eq ->
                let
                  (y, p) =
                    case eq of
                      Kind_eq k7 k8 -> (write_kind k7, write_kind k8)
                      Type_eq t7 t8 -> (write_type t7, write_type t8)
                in
                  Left (w2 ++ " mismatch between " ++ y ++ " and " ++ p ++ n))
              j
              (q8, x, g, i) >>=
            \(q9, y, p, (k2, k')) ->
              case Data.Set.null k2 of
                False -> Left ("Unresolved kind variables" ++ n)
                True ->
                  case Data.Set.null k' of
                    False -> Left ("Unresolved type variables" ++ n)
                    True ->
                      (
                        addargs w' p <$
                        traverse_ (check_cat (a l3) a3) q9 <*
                        slv
                          m
                          y
                          (\(Name p' q) -> \t -> \u ->
                            (
                              "Function " ++
                              q ++
                              location (a p') ++
                              " requires instance or constraint " ++
                              t ++
                              " " ++
                              u ++
                              ".")) <*
                        pats a ((\(Constructor _ _ _ _ e') -> e') <$> c) x3)))
  type_expression ::
    (
      (Map' PConstructor, Map' Constructor) ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      Expression_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err (Typedexpr, Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])]))
  type_expression (v', v) r (o', o) (Eqtns (f4, f) h mi c') d b e (r7, m8) =
    let
      ((o2, f2), t4) = new_typevar (o, f) star_kind
    in
      case b of
        Application_expression_1 c g ->
          (
            type_expression (v', v) r (o', o2) (Eqtns (f4, f2) h mi c') d c (function_type t4 e) (r7, m8) >>=
            \(i, j, p, d7) ->
              (\(l, m, q, e') -> (Application_texpr i l, m, q, d7 ++ e')) <$> type_expression (v', v) r p j d g t4 (r7, m8))
        Char_expression_1 c -> Right (Char_texpr c, Eqtns (f4, f) (Type_eq e char_type : h) mi c', (o', o), [])
        Function_expression_1 c g ->
          (
            type_pat r (v', v) c t4 d (o', o2) (f4, f2) h >>=
            \(a6, b6, (c2, c6), (f5, d6), f6) ->
              let
                ((c3, k5), t9) = new_typevar (c6, d6) star_kind
              in
                (
                  (\(a', b', d', f7) -> (Function_texpr a6 a', b', d', f7)) <$>
                  type_expression
                    (v', v)
                    r
                    (c2, c3)
                    (Eqtns (f5, k5) (Type_eq e (function_type t4 t9) : f6) mi c')
                    b6
                    g
                    t9
                    (r7, m8)))
        Int_expression_1 c -> Right (Int_texpr c, Eqtns (f4, f) (Type_eq e int_type : h) mi c', (o', o), [])
        Match_expression_1 a7 c g ->
          (
            type_expression (v', v) r (o', o2) (Eqtns (f4, f2) h mi c') d c t4 (r7, m8) >>=
            \(k, m, n, n2) ->
              (
                (\(q, u, x, n3, n4) -> (Match_texpr k q, u, x, n2 ++ [(a7, n4)] ++ n3)) <$>
                type_cases (v', v) r n m d g t4 e (r7, m8)))
        Name_expression_1 (Name a7 c) ->
          und_err
            c
            d
            "variable"
            (r a7)
            (\(Type_2 m2 i3 m3 i x0 a' j) ->
              let
                a8 = maybeToList m2 ++ i3
                o9 = o' + fromIntegral (length a8)
                f6 = show <$> [o' .. o9 - 1]
                d1 = Data.Map.fromList (zip a8 (Name_kind_1 <$> f6))
                ((f0, p, kl), u7) = typevar' v' (second (kindrep' d1) <$> i) (o, Data.Map.empty, f)
                x7 = (\(Constraint_1 a0 _ b0) -> (a0, (Name a7 c, p ! b0))) <$> a'
              in
                Right
                  (
                    case x0 of
                      Nothing -> Name_texpr_1 c (second snd <$> x7)
                      Just (Constraint_1 y0 _ _) -> Name_texpr_0 c y0 (head u7),
                    Eqtns
                      (Data.Set.union f4 (Data.Set.fromList f6), kl)
                      ([Type_eq e (repl' p (repkinds_type d1 j))] ++ h)
                      ((Name_kind_1 <$> m3) ++ mi)
                      (x7 ++ c'),
                    (o9, f0),
                    []))
  type_exprs ::
    (
      (Name -> String) ->
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Map' (Map' Inst) ->
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)] ->
      (Map' Expression_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      (Expression_2 -> Expression_2) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_exprs a b c d h i t z w f' t' t0 (x2, t4) t9 =
    case h of
      [] -> Right i
      (j @ (Name l2 y), k, s, t5, l) : m ->
        (
          type_expr
            (a j)
            (z l)
            b
            c
            k
            (type_constraints_1 t5 d t0)
            w
            t'
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' (pkind m0) k') x2 s, t4)
            t9
            l2 >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (f' g) i) t z w f' t' t0 (x2, t4) t9)
  type_kinds_1 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [(String, Kind_0)] ->
    Map' Polykind ->
    Map' Kind_1 ->
    Err ([(String, Kind_1)], Map' Polykind, Map' Kind_1)
  type_kinds_1 a b c d i =
    case c of
      [] -> Right ([], d, i)
      (e, f) : g ->
        (
          type_kind_7 a b Star_kind f >>=
          \h ->
            (
              (\(j, k, l) -> ((e, h) : j, k, l)) <$>
              type_kinds_1 a b g (Data.Map.insert e (pkind h) d) (Data.Map.insert e h i)))
  type_kt_1 ::
    (
      (Location_0 -> Location_1) ->
      ((Map' Kind, Map' Polykind, Map' Kind_1, Map' Cat_4), KT1) ->
      Err ((Map' Kind, Map' Polykind, Map' Kind_1, Map' Cat_4), KT2))
  type_kt_1 a ((b, e, f, u), KT1 c m d) =
    let
      g = Prelude.foldl (\h -> \i -> Data.Map.insert i Star_kind h) b c
    in
      (
        (\(n, t) -> \(j, k, l) -> ((g, k, l, n), KT2 c t j)) <$>
        type_cat_constrs (a, Data.Set.fromList c) (u, m) <*>
        type_kinds_1 a g d e f)
  type_pat ::
    (
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor) ->
      Pat' ->
      Type_1 ->
      Map' Type_2 ->
      (Integer, Integer) ->
      (Set String, Set String) ->
      [Eqtn] ->
      Err (Pat_1, Map' Type_2, (Integer, Integer), (Set String, Set String), [Eqtn]))
  type_pat a (b0, b) c d e f g h =
    case c of
      Application_pat' (Name i j) k ->
        und_err
          j
          b
          "constructor"
          (a i)
          (\(Constructor l m n o p) ->
            case p of
              [_] ->
                let
                  (q, r, s) = typevars b0 (l, m) (f, Data.Map.empty, g)
                in
                  (
                    (\(t, u, v, w, x) -> (Application_pat_1 t, u, v, w, x)) <$>
                    type_pats a (b0, b) k (repl' r <$> n) e q s (Type_eq d (repl' r o) : h) (Name i j))
              _ -> Left ("Constructor " ++ j ++ location (a i) ++ " is not a struct constructor."))
      Blank_pat' -> Right (Blank_pat_1, e, f, g, h)
      Name_pat' i -> Right (Name_pat_1 i, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] d) e, f, g, h)
  type_pats ::
    (
      (Location_0 -> Location_1) ->
      (Map' PConstructor, Map' Constructor) ->
      [Pat'] ->
      [Type_1] ->
      Map' Type_2 ->
      (Integer, Integer) ->
      (Set String, Set String) ->
      [Eqtn] ->
      Name ->
      Err ([Pat_1], Map' Type_2, (Integer, Integer), (Set String, Set String), [Eqtn]))
  type_pats a b d e f g h i (Name x y) =
    let
      z a' = Left ("Constructor " ++ y ++ location (a x) ++ " has been given too " ++ a' ++ " arguments.")
    in
      case d of 
        [] ->
          case e of
            [] -> Right ([], f, g, h, i)
            _ -> z "few"
        j : k ->
          case e of
            [] -> z "many"
            m : n ->
              (
                type_pat a b j m f g h i >>=
                \(c, o, p, q, r) -> (\(s, t, u, v, w) -> (c : s, t, u, v, w)) <$> type_pats a b k n o p q r (Name x y))
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b _ _ -> (b, d)
  typevar ::
    Map' PConstructor -> (TPat, Kind_1) -> (Integer, Map' Type_1, Set String) -> ((Integer, Map' Type_1, Set String), Type_1)
  typevar j (a, i) (b, c, d) =
    case a of
      Application_tpat f e ->
        let
          PConstructor _ h k _ = j ! f
        in
          second
            (Prelude.foldl Application_type_1 (ntype f))
            (typevar' j (zip e (kindrep' (skinds k i) <$> h)) (b, c, d))
      Name_tpat e ->
        let
          ((f, g), h) = new_typevar (b, d) i
        in
          ((f, Data.Map.insert e h c, g), h)
  typevar' ::
    Map' PConstructor -> [(TPat, Kind_1)] -> (Integer, Map' Type_1, Set String) -> ((Integer, Map' Type_1, Set String), [Type_1])
  typevar' f a b =
    case a of
      [] -> (b, [])
      c : d ->
        let
          (e, g) = typevar f c b
        in
          second ((:) g) (typevar' f d e)
  typevars ::
    (
      Map' PConstructor ->
      ([String], [(TPat, Kind_1)]) ->
      ((Integer, Integer), Map' Type_1, (Set String, Set String)) ->
      ((Integer, Integer), Map' Type_1, (Set String, Set String)))
  typevars m (a, b) ((c, d), e, (f, g)) =
    let
      (h, i) = kindvars a (c, f)
      ((j, k, l), _) = typevar' m b (d, e, g)
    in
      ((h, j), k, (i, l))
  write_kind :: Kind_1 -> String
  write_kind a = fst (write_kind' a)
  write_kind' :: Kind_1 -> (String, Bool)
  write_kind' a =
    case a of
      Application_kind_1 b c ->
        let
          (d, e) = write_kind' c
        in
          (
            (
              write_kind b ++
              " " ++
              case e of
                False -> d
                True -> "(" ++ d ++ ")"),
            True)
      Name_kind_1 b -> (b, False)
  write_type :: Type_1 -> String
  write_type a = fst (write_type' a)
  write_type' :: Type_1 -> (String, Bool)
  write_type' a =
    case a of
      Application_type_1 b c ->
        let
          (d, e) = write_type' c
        in
          (
            (
              write_type b ++
              " " ++
              case e of
                False -> d
                True -> "(" ++ d ++ ")"),
            True)
      Name_type_1 b c d ->
        (
          (
            b ++
            (case c of
              Nothing -> ""
              Just e -> " {{" ++ write_kind e ++ "}}") ++
            case d of
              [] -> ""
              _ -> " [[" ++ intercalate ", " (write_kind <$> d) ++ "]]"),
          False)
  ziph ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind ->
      String ->
      Location_0 ->
      (Maybe String, [String]) ->
      (Maybe Kind_0, [Kind_0]) ->
      Err ((Maybe Kind_1, [Kind_1]), Map' Kind_1))
  ziph a b c d (f, g) (h, i) =
    case (f, h) of
      (Nothing, Nothing) -> first ((,) Nothing) <$> ziphelp a b c d Data.Map.empty g i
      (Nothing, Just _) -> Left ("Illegal ad hoc kind argument for " ++ c ++ location' (a d))
      (Just _, Nothing) -> Left ("Missing ad hoc kind argument for " ++ c ++ location' (a d))
      (Just j, Just k) ->
        type_kind_7 a b Star_kind k >>= \l -> first ((,) (Just l)) <$> ziphelp a b c d (Data.Map.singleton j l) g i
  ziphelp ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    String ->
    Location_0 ->
    Map' Kind_1 ->
    [String] ->
    [Kind_0] ->
    Err ([Kind_1], Map' Kind_1)
  ziphelp l k f a d b c =
    let
      e = Left ("Wrong number of kind arguments for " ++ f ++ location' (l a))
    in
      case b of
        [] ->
          case c of
            [] -> Right ([], d)
            _ -> e
        g : h ->
          case c of
            [] -> e
            i : j -> type_kind_7 l k Star_kind i >>= \m -> first ((:) m) <$> ziphelp l k f a (Data.Map.insert g m d) h j
--------------------------------------------------------------------------------------------------------------------------------