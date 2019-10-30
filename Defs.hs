--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Defs where
  import Cats_0
  import Cats_1
  import Classes_0
  import Classes_1
  import Control.Monad
  import Control.Monad.Trans.State.Strict
  import Data.Bifunctor
  import Data.Foldable
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Datas_0
  import Datas_1
  import Datas_2
  import Defs_and_instances_0
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_3 = Blank_alg_pat_3 | Int_alg_pat_3 Integer | Struct_alg_pat_3 String [Alg_pat_3] deriving Show
  data Eqtn = Kind_eq Kind_1 Kind_1 | Type_eq Type_1 Type_1 deriving Show
  data Eqtns = Eqtns (Set String, Set String) [Eqtn] [(Type_1, Kind_1)] [Kind_1] [(String, (Name, Type_1))] deriving Show
  data Pattern_5 =
    Blank_pattern_5 | Int_blank_pattern_5 (Set Integer) | Int_pattern_5 Integer | Struct_pattern_5 String [Pattern_5]
      deriving Show
  data Typedexpr =
    Application_texpr Typedexpr Typedexpr |
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
  get_kind :: Map' Polykind -> Type_1 -> Kind_1
  get_kind f a =
    case a of
      Application_type_1 b _ ->
        case get_kind f b of
          Application_kind_1 _ c -> c
          _ -> undefined
      Name_type_1 c d e ->
        let
          Polykind g h i = f ! c
        in
          (kindrep' (Data.Map.fromList (maybeToList ((,) <$> g <*> d) ++ zip h e))) i
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      Alg_pat_1 ->
      Type_1 ->
      StateT
        ((Integer, Integer), Map' Type_2)
        Err
        ((Set String, Set String), [Eqtn], (Alg_pat_2, Alg_pat_3), [(Type_1, Kind_1)]))
  get_pattern_type a (f3, b', b) g h  =
    case g of
      Application_alg_pat_1 (Name o i) j ->
        StateT
          (\(d, n) ->
            und_err
              i
              b
              "constructor"
              (a o)
              (\(Constructor k u1 x m _) ->
                let
                  ((q, r, s, t2), k8) =
                    typevars (f3, b') (k, u1) (d, Data.Map.empty, (Data.Set.empty, Data.Set.empty), Data.Map.empty)
                  f2 x1 = type_rep (kindrep' t2) (repl' r x1)
                in
                  runStateT
                    (
                      (\(a3, a5, a6, a7, n9) ->
                        (
                          s_union s a3,
                          Type_eq h (f2 m) : a5,
                          (Application_alg_pat_2 i a6, Struct_alg_pat_3 i a7),
                          k8 ++ n9)) <$>
                      get_pattern_types a (f3, b', b) j (f2 <$> x) (Name o i))
                    (q, n)))
      Blank_alg_pat_1 -> return ((Data.Set.empty, Data.Set.empty), [], (Blank_alg_pat_2, Blank_alg_pat_3), [])
      Int_alg_pat_1 i -> return ((Data.Set.empty, Data.Set.empty), [Type_eq h int_type], (Int_alg_pat_2 i, Int_alg_pat_3 i), [])
      Name_alg_pat_1 i ->
        StateT
          (\(d, n) ->
            Right
              (
                ((Data.Set.empty, Data.Set.empty), [], (Name_alg_pat_2 i, Blank_alg_pat_3), []),
                (d, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] h) n)))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      StateT
        ((Integer, Integer), Map' Type_2)
        Err
        ((Set String, Set String), [Eqtn], [Alg_pat_2], [Alg_pat_3], [(Type_1, Kind_1)]))
  get_pattern_types a b e f (Name m n) =
    case (e, f) of
      ([], []) -> return ((Data.Set.empty, Data.Set.empty), [], [], [], [])
      (g : h, i : j) ->
        (
          (\(k1, k2, (l, t), a4) -> \(a3, a5, a6, a7, y7) -> (s_union k1 a3, k2 ++ a5, l : a6, t : a7, a4 ++ y7)) <$>
          get_pattern_type a b g i <*>
          get_pattern_types a b h j (Name m n))
      _ -> StateT (return (Left ("Constructor " ++ n ++ location (a m) ++ " has been given a wrong number of arguments.")))
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
    Data.Map.fromList [("Ord", Data.Map.fromList [("Int", Inst [] [])]), ("Ring", Data.Map.fromList [("Int", Inst [] [])])]
  jeqs :: Eqtns -> Eqtns -> Eqtns
  jeqs (Eqtns g a b c i) (Eqtns h d e f j) = Eqtns (s_union g h) (a ++ d) (b ++ e) (c ++ f) (i ++ j)
  kindvar :: String -> (Integer, Set String, Map' Kind_1) -> (Integer, Set String, Map' Kind_1)
  kindvar a (b, c, e) =
    let
      d = show b
    in
      (b + 1, Data.Set.insert d c, Data.Map.insert a (Name_kind_1 d) e)
  kindvars :: [String] -> (Integer, Set String, Map' Kind_1) -> (Integer, Set String, Map' Kind_1)
  kindvars a b =
    case a of
      [] -> b
      d : e -> kindvars e (kindvar d b)
  krep_eq :: (Kind_1 -> Kind_1) -> Eqtn -> Eqtn
  krep_eq a b =
    case b of
      Kind_eq c d -> Kind_eq (a c) (a d)
      Type_eq c d -> Type_eq (type_rep a c) (type_rep a d)
  new_typevar :: Map' Prom_alg -> (Integer, Set String) -> Kind_1 -> ((Integer, Set String), Type_1, [(Type_1, Kind_1)])
  new_typevar e (a, b) d =
    let
      c = show a
      (f, g) = kind_string d []
      u1 = ntype c
      h = ((a + 1, Data.Set.insert c b), u1, [(u1, d)])
    in
      case Data.Map.lookup f e of
        Nothing -> h
        Just (Prom_alg m i) ->
          case assocs i of
            [(k, l)] ->
              let
                (u2, f7, k7) = new_typevars e (kindrep' (Data.Map.fromList (zip m g)) <$> l) (a, b)
              in
                (u2, Prelude.foldl Application_type_1 (Name_type_1 k Nothing g) f7, k7)
            _ -> h
  new_typevars :: Map' Prom_alg -> [Kind_1] -> (Integer, Set String) -> ((Integer, Set String), [Type_1], [(Type_1, Kind_1)])
  new_typevars a c b =
    case c of
      [] -> (b, [], [])
      d : e ->
        let
          (f, g, t) = new_typevar a b d
          (m, n, l) = new_typevars a e f
        in
          (m, g : n, t ++ l)
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
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c _ _ ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
  s_union :: (Set String, Set String) -> (Set String, Set String) -> (Set String, Set String)
  s_union (a, b) (c, d) = (Data.Set.union a c, Data.Set.union b d)
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
  solve_kvars ::
    (
      Map' Polykind ->
      [Either (Type_1, Kind_1) (Kind_1, Kind_1)] ->
      (Set String, [Kind_1], [(String, (Name, Type_1))], Typedexpr) ->
      (Set String, [Kind_1], [(String, (Name, Type_1))], Typedexpr))
  solve_kvars l a (b, c, d, e) =
    case a of
      [] -> (b, c, d, e)
      f : g ->
        case f of
          Left (h, i) -> solve_kvars l (Right (i, get_kind l h) : g) (b, c, d, e)
          Right h ->
            case h of
              (Application_kind_1 j k, Application_kind_1 n m) -> solve_kvars l (Right (j, n) : Right (k, m) : g) (b, c, d, e)
              (Name_kind_1 j, Name_kind_1 k) ->
                case j == k of
                  False ->
                    case (Data.Set.member j b, Data.Set.member k b) of
                      (True, _) -> solve_kvars' l j (Name_kind_1 k) g (b, c, d, e)
                      (_, True) -> solve_kvars' l k (Name_kind_1 j) g (b, c, d, e)
                      _ -> undefined
                  True -> solve_kvars l g (b, c, d, e)
              (Name_kind_1 i, j) -> solve_kvars' l i j g (b, c, d, e)
              (i, Name_kind_1 j) -> solve_kvars' l j i g (b, c, d, e)
  solve_kvars' ::
    (
      Map' Polykind ->
      String ->
      Kind_1 ->
      [Either (Type_1, Kind_1) (Kind_1, Kind_1)] ->
      (Set String, [Kind_1], [(String, (Name, Type_1))], Typedexpr) ->
      (Set String, [Kind_1], [(String, (Name, Type_1))], Typedexpr))
  solve_kvars' j a b c (d, e, f, g) =
    let
      h = kindrep a b
      i = type_rep h
    in
      solve_kvars j (bimap (bimap i h) (bimap h h) <$> c) (Data.Set.delete a d, h <$> e, second (second i) <$> f, sysrep3 h g)
  solvek' ::
    (
      (
        String ->
        Eqtn ->
        Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))) ->
      Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      Eqtn ->
      StateT ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) Err ())
  solvek' m3 h b c d eq =
    StateT
      (\(g4, y, x, m, (w, a)) ->
        case Data.Set.member b w of
          False -> h
          True -> runStateT (solvek_rep m3 h b c d) (g4, y, x, m, (w, a)))
  solvek_rep ::
    (
      (
        String ->
        Eqtn ->
        Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))) ->
      Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      StateT ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) Err ())
  solvek_rep m3 a c d e =
    let
      m = kindrep c d
      a2 = type_rep m
    in
      case occ_k c d of
        False ->
          StateT
            (\(t4, y, x, f, (w, k)) ->
              runStateT
                (solvesys m3 (krep_eq m <$> e))
                (bimap a2 m <$> t4, m <$> y, second (second a2) <$> x, sysrep3 m f, (Data.Set.delete c w, k)))
        True -> StateT (return a)
  solvesys ::
    (
      (
        String ->
        Eqtn ->
        Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))) ->
      [Eqtn] ->
      StateT ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) Err ())
  solvesys m b =
    case b of
      [] -> return ()
      eq : g ->
        let
          e3 = m "Kind" eq
          e4 = m "Type" eq
        in
          case eq of
            Kind_eq c d ->
              case (c, d) of
                (Application_kind_1 e f, Application_kind_1 h i) -> solvesys m (Kind_eq e h : Kind_eq f i : g)
                (Name_kind_1 e, Name_kind_1 f) ->
                  case e == f of
                    False ->
                      StateT
                        (\(m3, a2, a', t, (w, u)) ->
                          case (Data.Set.member e w, Data.Set.member f w) of
                            (False, False) -> e3
                            (True, _) -> runStateT (solvek_rep m e3 e d g) (m3, a2, a', t, (w, u))
                            (_, True) -> runStateT (solvek_rep m e3 f c g) (m3, a2, a', t, (w, u)))
                    True -> solvesys m g
                (Name_kind_1 e, _) -> solvek' m e3 e d g eq
                (_, Name_kind_1 e) -> solvek' m e3 e c g eq
            Type_eq c d ->
              case (c, d) of
                (Application_type_1 e f, Application_type_1 h i) -> solvesys m (Type_eq e h : Type_eq f i : g)
                (Name_type_1 e e0 e1, Name_type_1 f f0 f1) ->
                  case e == f of
                    False ->
                      StateT
                        (\(m3, a2, a', t, (w, u)) ->
                          case (Data.Set.member e u, Data.Set.member f u) of
                            (False, False) -> e4
                            (True, _) -> runStateT (solvesys_rep m e4 e d g) (m3, a2, a', t, (w, u))
                            (_, True) -> runStateT (solvesys_rep m e4 f c g) (m3, a2, a', t, (w, u)))
                    True -> solvesys m (maybeToList (Kind_eq <$> e0 <*> f0) ++ zipWith Kind_eq e1 f1 ++ g)
                (Name_type_1 e _ _, _) -> StateT (solvesys' m e4 e d g)
                (_, Name_type_1 e _ _) -> StateT (solvesys' m e4 e c g)
  solvesys' ::
    (
      (
        String ->
        Eqtn ->
        Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))) ->
      Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))))
  solvesys' m3 h b c d (x2, y, x, m, (w, a)) =
    case Data.Set.member b a of
      False -> h
      True -> runStateT (solvesys_rep m3 h b c d) (x2, y, x, m, (w, a))
  solvesys_rep ::
    (
      (
        String ->
        Eqtn ->
        Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))) ->
      Err ((), ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      StateT ([(Type_1, Kind_1)], [Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) Err ())
  solvesys_rep m3 a c d e =
    let
      m = sysrep' c d
    in
      case occ_check c d of
        False ->
          StateT
            (\(y2, y, x, f, (w, k)) ->
              runStateT
                (solvesys m3 (rep_eq m <$> e))
                (first m <$> y2, y, second (second m) <$> x, sysrep2 c d f, (w, Data.Set.delete c k)))
        True -> StateT (return a)
  split_pattern :: Map' [(String, Integer)] -> Pattern_5 -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  split_pattern context x y =
    case (x, y) of
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
  sysrep3 :: (Kind_1 -> Kind_1) -> Typedexpr -> Typedexpr
  sysrep3 a b =
    let
      c = sysrep3 a
      g = type_rep a
    in
      case b of
        Application_texpr d e -> Application_texpr (c d) (c e)
        Function_texpr d e -> Function_texpr d (c e)
        Match_texpr d e -> Match_texpr (c d) (second c <$> e)
        Name_texpr_0 d e f -> Name_texpr_0 d e (g f)
        Name_texpr_1 d e -> Name_texpr_1 d (second g <$> e)
        _ -> b
  type_case ::
    (
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      String ->
      Map' Type_2 ->
      (Alg_pat_1, Expression_1) ->
      Type_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      StateT (Integer, Integer) Err ((Alg_pat_2, Typedexpr), Eqtns, [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a c f (g, h) i j k =
    StateT
      (\d ->
        (
          runStateT (get_pattern_type (Location_1 c) a g i) (d, f) >>=
          \((n, s, (o, y), d3), (m, t)) ->
            runStateT ((\(u, v, r0) -> ((o, u), jeqs (Eqtns n s d3 [] []) v, r0, y)) <$> type_expression a c t h j k) m))
  type_cases ::
    (
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      String ->
      Map' Type_2 ->
      [(Alg_pat_1, Expression_1)] ->
      Type_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      StateT (Integer, Integer) Err ([(Alg_pat_2, Typedexpr)], Eqtns, [(Location_0, [Alg_pat_3])], [Alg_pat_3]))
  type_cases b c f g n h i =
    case g of
      [] -> return ([], Eqtns (Data.Set.empty, Data.Set.empty) [] [] [] [], [], [])
      l : m ->
        (
          type_case b c f l n h i >>=
          \(o, p, r0, r') -> (\(r, s, r1, w) -> (o : r, jeqs p s, r0 ++ r1, r' : w)) <$> type_cases b c f m n h i)
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
  type_def_2 ::
    (
      String ->
      Def_4 ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Map' (Map' Inst) ->
      Map' Polykind ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      Map' Kind ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_def_2 j a d m n t' u0 v2 f1 =
    case a of
      Basic_def_4 r e (KT2 b0 g3 b) x h i y' ->
        (
          (\t -> Data.Map.singleton e (Prelude.foldr (\x' -> Function_expression_2 (Name_pat_1 x')) t y')) <$>
          type_expr
            ("definition " ++ e ++ location' (Location_1 j r))
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
              (\(Name x g) -> "definition " ++ g ++ " " ++ e ++ location' (Location_1 j x))
              j
              d
              (type_constraints_1 g' m u0)
              f
              e
              (sysrep' w f')
              e1
              (\g -> Prelude.foldr (\b -> Function_expression_2 (Name_pat_1 b)) g c2)
              t'
              u0
              (f4, v2)
              f1
          s' w1 =
            Left (e' ++ " " ++ e ++ " at" ++ location (Location_1 j l') ++ " is an illegal instance because " ++ w1 ++ ".")
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
                          check_cats_2 (Location_1 j l') f1 u4 *>
                          case constr_check u0 (fst <$> e0) u r' of
                            Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                            Nothing -> r)
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
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
  type_defs_2 ::
    String ->
    [Def_4] ->
    (Map' Prom_alg, Map' PConstructor, Map' Constructor, Map' Type_2) ->
    Map' (Map' Inst) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Kind ->
    Map' Cat_4 ->
    Err (Map' Expression_2)
  type_defs_2 f a b g i j u v h' =
    case a of
      [] -> Right Data.Map.empty
      d : e -> Data.Map.union <$> type_def_2 f d b g i j u v h' <*> type_defs_2 f e b g i j u v h'
  type_expr ::
    (
      String ->
      Type_1 ->
      String ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Expression_1 ->
      Map' (Map' Inst) ->
      Integer ->
      Map' ([String], Map' [(String, Nat)]) ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Location_0 ->
      Err Expression_2)
  type_expr k h a (a4, c4, c, e) f m w w' (b, b5) a3 l3 =
    let
      n = " in " ++ k
    in
      (
        runStateT (type_expression (a4, c4, c) a e f h (b, b5)) (0, w) >>=
        \((g, Eqtns i j f7 q8 x, x3), _) ->
          (
            runStateT
              (solvesys
                (\w2 -> \eq ->
                  let
                    (y, p) =
                      case eq of
                        Kind_eq k7 k8 -> (write_kind k7, write_kind k8)
                        Type_eq t7 t8 -> (write_type t7, write_type t8)
                  in
                    Left (w2 ++ " mismatch between " ++ y ++ " and " ++ p ++ n))
                j)
              (f7, q8, x, g, i) >>=
            \((), (t3, b7, m8, p2, (g7, k'))) ->
              case Data.Set.null k' of
                False -> Left ("Unresolved type variables" ++ n)
                True ->
                  let
                    (k2, q9, y, p) = solve_kvars b (Left <$> t3) (g7, b7, m8, p2)
                  in
                    case Data.Set.null k2 of
                      False -> Left ("Unresolved kind variables" ++ n)
                      True ->
                        (
                          addargs w' p <$
                          traverse_ (check_cat (Location_1 a l3) a3) q9 <*
                          slv
                            m
                            y
                            (\(Name p' q) -> \t -> \u ->
                              (
                                "Function " ++
                                q ++
                                location (Location_1 a p') ++
                                " requires instance or constraint " ++
                                t ++
                                " " ++
                                u ++
                                ".")) <*
                          pats (Location_1 a) ((\(Constructor _ _ _ _ e') -> e') <$> c) x3)))
  type_expression ::
    (
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      String ->
      Map' Type_2 ->
      Expression_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      StateT (Integer, Integer) Err (Typedexpr, Eqtns, [(Location_0, [Alg_pat_3])]))
  type_expression (x1, v', v) r d b e r7 =
    case b of
      Application_expression_1 c g ->
        StateT
          (\(o', o) ->
            let
              ((o2, f2), t4, _) = new_typevar x1 (o, Data.Set.empty) star_kind
            in
              (
                runStateT (type_expression (x1, v', v) r d c (function_type t4 e) r7) (o', o2) >>=
                \((i, j, d7), p) ->
                  runStateT
                    (
                      (\(l, m, e') ->
                        (Application_texpr i l, jeqs (Eqtns (Data.Set.empty, f2) [] [] [] []) (jeqs j m), d7 ++ e')) <$>
                      type_expression (x1, v', v) r d g t4 r7)
                    p))
      Function_expression_1 c g ->
        StateT
          (\(o', o) ->
            let
              ((o2, f2), t4, _) = new_typevar x1 (o, Data.Set.empty) star_kind
              ((a, h), i, _) = new_typevar x1 (o2, f2) star_kind
            in
              (
                runStateT (type_pat r (x1, v', v) c t4) ((o', a), d) >>=
                \((j, m, n, n4), (l, k)) ->
                  runStateT
                    (
                      (\(p, s, u) ->
                        (
                          Function_texpr j p,
                          jeqs (Eqtns (s_union (Data.Set.empty, h) m) (Type_eq e (function_type t4 i) : n) n4 [] []) s,
                          u)) <$>
                      type_expression (x1, v', v) r k g i r7)
                    l))
      Int_expression_1 c -> return (Int_texpr c, Eqtns (Data.Set.empty, Data.Set.empty) [Type_eq e int_type] [] [] [], [])
      Match_expression_1 a7 c g ->
        StateT
          (\(o', o) ->
            let
              ((o2, f2), t4, _) = new_typevar x1 (o, Data.Set.empty) star_kind
            in
              runStateT
                (
                  type_expression (x1, v', v) r d c t4 r7 >>=
                  \(k, m, n2) ->
                    (
                      (\(q, u, n3, n4) ->
                        (Match_texpr k q, jeqs (Eqtns (Data.Set.empty, f2) [] [] [] []) (jeqs m u), n2 ++ [(a7, n4)] ++ n3)) <$>
                      type_cases (x1, v', v) r d g t4 e r7))
                (o', o2))
      Name_expression_1 (Name a7 c) ->
        StateT
          (\(o', o) ->
            und_err
              c
              d
              "variable"
              (Location_1 r a7)
              (\(Type_2 m2 i3 m3 i x0 a' j) ->
                let
                  a8 = maybeToList m2 ++ i3
                  o9 = o' + fromIntegral (length a8)
                  f6 = show <$> [o' .. o9 - 1]
                  d1 = Data.Map.fromList (zip a8 (Name_kind_1 <$> f6))
                  ((f0, p, kl), u7, m5) = typevar' (x1, v') (second (kindrep' d1) <$> i) (o, Data.Map.empty, Data.Set.empty)
                  x7 = (\(Constraint_1 a0 _ b0) -> (a0, (Name a7 c, p ! b0))) <$> a'
                in
                  Right
                    (
                      (
                        case x0 of
                          Nothing -> Name_texpr_1 c (second snd <$> x7)
                          Just (Constraint_1 y0 _ _) -> Name_texpr_0 c y0 (head u7),
                        Eqtns
                          (Data.Set.fromList f6, kl)
                          [Type_eq e (repl' p (repkinds_type d1 j))]
                          m5
                          (kindrep' d1 <$> Name_kind_1 <$> m3)
                          x7,
                        []),
                      (o9, f0))))
  type_exprs ::
    (
      (Name -> String) ->
      String ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor, Map' Type_2) ->
      Map' (Map' Inst) ->
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)] ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      (Expression_2 -> Expression_2) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_exprs a b c d h t z w f' t' t0 (x2, t4) t9 =
    case h of
      [] -> Right Data.Map.empty
      (j @ (Name l2 y), k, s, t5, l) : m ->
        (
          (\g -> Data.Map.insert (y ++ " " ++ t) (f' g)) <$>
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
            l2 <*>
          type_exprs a b c d m t z w f' t' t0 (x2, t4) t9)
  type_pat ::
    (
      String ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      Pat' ->
      Type_1 ->
      StateT ((Integer, Integer), Map' Type_2) Err (Pat_1, (Set String, Set String), [Eqtn], [(Type_1, Kind_1)]))
  type_pat a (d2, b0, b) c d  =
    case c of
      Application_pat' (Name i j) k ->
        StateT
          (\(f, e) ->
            und_err
              j
              b
              "constructor"
              (Location_1 a i)
              (\(Constructor l m n o p) ->
                case p of
                  [_] ->
                    let
                      ((q, r, s, t2), m5) =
                        typevars (d2, b0) (l, m) (f, Data.Map.empty, (Data.Set.empty, Data.Set.empty), Data.Map.empty)
                      f2 x1 = type_rep (kindrep' t2) (repl' r x1)
                    in
                      runStateT
                        (
                          (\(t, w, x, f4) -> (Application_pat_1 t, s_union s w, Type_eq d (f2 o) : x, m5 ++ f4)) <$>
                          type_pats a (d2, b0, b) k (f2 <$> n) (Name i j))
                        (q, e)
                  _ -> Left ("Constructor " ++ j ++ location (Location_1 a i) ++ " is not a struct constructor.")))
      Blank_pat' -> return (Blank_pat_1, (Data.Set.empty, Data.Set.empty), [], [])
      Name_pat' i ->
        StateT
          (\(f, e) ->
            Right
              (
                (Name_pat_1 i, (Data.Set.empty, Data.Set.empty), [], []),
                (f, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] d) e)))
  type_pats ::
    (
      String ->
      (Map' Prom_alg, Map' PConstructor, Map' Constructor) ->
      [Pat'] ->
      [Type_1] ->
      Name ->
      StateT ((Integer, Integer), Map' Type_2) Err ([Pat_1], (Set String, Set String), [Eqtn], [(Type_1, Kind_1)]))
  type_pats a b d e (Name x y) =
    StateT
      (\(g, f) ->
        let
          z a' = Left ("Constructor " ++ y ++ location (Location_1 a x) ++ " has been given too " ++ a' ++ " arguments.")
        in
          case d of
            [] ->
              case e of
                [] -> Right (([], (Data.Set.empty, Data.Set.empty), [], []), (g, f))
                _ -> z "few"
            j : k ->
              case e of
                [] -> z "many"
                m : n ->
                  (
                    runStateT (type_pat a b j m) (g, f) >>=
                    \((c, q, r, d3), o) ->
                      runStateT
                        ((\(s, v, w, w3) -> (c : s, s_union q v, r ++ w, d3 ++ w3)) <$> type_pats a b k n (Name x y))
                        o))
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b _ _ -> (b, d)
  typevar ::
    (
      (Map' Prom_alg, Map' PConstructor) ->
      (TPat, Kind_1) ->
      (Integer, Map' Type_1, Set String) ->
      ((Integer, Map' Type_1, Set String), Type_1, [(Type_1, Kind_1)]))
  typevar (l, j) (a, i) (b, c, d) =
    case a of
      Application_tpat f e ->
        let
          PConstructor _ h k _ = j ! f
          (g, m, n) = typevar' (l, j) (zip e (kindrep' (skinds k i) <$> h)) (b, c, d)
        in
          (g, Prelude.foldl Application_type_1 (ntype f) m, n)
      Name_tpat e ->
        let
          ((f, g), h, m) = new_typevar l (b, d) i
        in
          ((f, Data.Map.insert e h c, g), h, m)
  typevar' ::
    (
      (Map' Prom_alg, Map' PConstructor) ->
      [(TPat, Kind_1)] ->
      (Integer, Map' Type_1, Set String) ->
      ((Integer, Map' Type_1, Set String), [Type_1], [(Type_1, Kind_1)]))
  typevar' f a b =
    case a of
      [] -> (b, [], [])
      c : d ->
        let
          (e, g, h) = typevar f c b
          (i, j, k) = typevar' f d e
        in
          (i, g : j, h ++ k)
  typevars ::
    (
      (Map' Prom_alg, Map' PConstructor) ->
      ([String], [(TPat, Kind_1)]) ->
      ((Integer, Integer), Map' Type_1, (Set String, Set String), Map' Kind_1) ->
      (((Integer, Integer), Map' Type_1, (Set String, Set String), Map' Kind_1), [(Type_1, Kind_1)]))
  typevars m (a, b) ((c, d), e, (f, g), y) =
    let
      (h, i, x) = kindvars a (c, f, y)
      ((j, k, l), _, m5) = typevar' m (second (kindrep' x) <$> b) (d, e, g)
    in
      (((h, j), k, (i, l), x), m5)
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
--------------------------------------------------------------------------------------------------------------------------------