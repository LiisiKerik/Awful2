--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Classes where
  import Control.Monad
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Set
  import Datas
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Class_3 = Class_3 String [String] [String] (String, Kind_1) (Maybe (Name, [Kind_1])) [Method_3] deriving Show
  data Class_4 = Class_4 [String] [String] (String, Kind_1) (Maybe (String, [Kind_1])) [Method_4] deriving Show
  data Class_5 = Class_5 [String] [String] Kind_1 (Maybe (String, [Kind_1])) [String] deriving Show
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_1)] [Constraint_1] Type_1 deriving Show
  chain_constraints :: Maybe String -> Map' Class_5 -> Map' (Map' [String]) -> String -> Map' (Map' [String])
  chain_constraints a b c e =
    case a of
      Just d ->
        let
          Class_5 _ _ _ f o = b ! d
        in
          chain_constraints
            (fst <$> f)
            b
            (Data.Map.insert
              d
              (Data.Map.insert
                e 
                ((\u -> u ++ " " ++ e) <$> o)
                (case Data.Map.lookup d c of
                  Just g -> g
                  Nothing -> Data.Map.empty))
              c)
            e
      Nothing -> c
  check_cats :: Location_1 -> Map' Cat_4 -> Map' Kind_1 -> [String] -> Err ()
  check_cats f e a b =
    case b of
      [] -> Right ()
      c : d -> check_cat f e (a ! c) *> check_cats f e a d
  classes_0 :: Map' Class_4
  classes_0 =
    Data.Map.fromList
      [
        (
          "Ord",
          Class_4
            []
            []
            ("T", star_kind)
            Nothing
            [Method_4 "compare" [] [] (function_type (ntype "T") (function_type (ntype "T") comparison_type))]),
        (
          "Ring",
          Class_4
            []
            []
            ("T", star_kind)
            Nothing
            [
              Method_4 "add" [] [] (function_type (ntype "T") (function_type (ntype "T") (ntype "T"))),
              Method_4 "convert" [] [] (function_type int_type (ntype "T")),
              Method_4 "times" [] [] (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 e f (_, a) b c) -> Class_5 e f a b ((\(Method_4 d _ _ _) -> d) <$> c)) <$> classes_0
  classes_2 :: Map' ([String], [String], Kind_1)
  classes_2 = (\(Class_4 b c (_, a) _ _) -> (b, c, a)) <$> classes_0
  type_class_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind ->
      Map' Polykind ->
      Class_2 ->
      Map' Cat_4 ->
      (
        Map' ([String], Map' [(String, Nat)]),
        Map' (([String], [String], Kind_1), Status),
        Map' (Class_5, Status),
        Map' String) ->
      Err
        (
          Class_3,
          (
            Map' ([String], Map' [(String, Nat)]),
            Map' (([String], [String], Kind_1), Status),
            Map' (Class_5, Status),
            Map' String)))
  type_class_0 a i j (Class_2 b h0 h2 (c, d) g' e) e7 (w0, i', j0, x2) =
    let
      i2 = Prelude.foldl (\t4 -> \b2 -> Data.Map.insert b2 Star_kind t4) i h0
    in
      (
        type_cat_constrs (a, Data.Set.fromList h0) (e7, h2) >>=
        \(m9, t8) ->
          (
            type_kind_7 a i2 Star_kind d >>=
            \h ->
              (
                type_inh b [b] ((\(Constraint_0 (Name _ t4) _ _) -> t4) <$> g') x2 *>
                (
                  (case g' of
                    Nothing -> Right Nothing
                    Just (Constraint_0 t4 m2 (Name d3 f5)) ->
                      (
                        (\x3 -> Just (t4, x3)) <$>
                        traverse (type_kind_7 a i Star_kind) m2 <*
                        case c == f5 of
                          False -> Left ("Undefined type variable " ++ f5 ++ location' (a d3))
                          True -> Right ())) >>=
                  \g3 ->
                    (
                      (\g ->
                        let
                          g2 = (\(Method_3 w1 _ _ _) -> w1) <$> g
                        in
                          (
                            Class_3 b h0 t8 (c, h) g3 g,
                            (
                              Data.Map.insert b (g2, Data.Map.empty) w0,
                              ins_new b (h0, t8, h) i',
                              ins_new b (Class_5 h0 t8 h (first (\(Name _ t4) -> t4) <$> g3) g2) j0,
                              case g' of
                                Just (Constraint_0 (Name _ t0) _ _) -> Data.Map.insert b t0 x2
                                Nothing -> x2))) <$>
                      type_methods_0 a e (Data.Map.insert c (pkind h) j) i2 m9)))))
  type_class_1 ::
    (
      String ->
      Class_3 ->
      Map' Kind ->
      Map' ([String], [String], Kind_1) ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Type_2, Status), Map' (Class_4, Status)) ->
      Err (Map' (Type_2, Status), Map' (Class_4, Status)))
  type_class_1 a (Class_3 b g1 g7 (c, k) g' e) u7 d f1 c4 (f0, f) =
    let
      x1 = Constraint_1 b (Name_kind_1 <$> g1) c
      l m =
        (
          (\e' ->
            (
              Prelude.foldl
                (\x -> \(Method_4 t s u0 u) ->
                  ins_new t (Type_2 Nothing g1 g7 (first Name_tpat <$> ((c, k) : s)) (Just x1) (x1 : u0) u) x)
                f0
                e',
              ins_new b (Class_4 g1 g7 (c, k) m e') f)) <$>
          type_methods_1 a (u7, f1, c4) e)
    in
      case g' of
        Just (Name m g, x2) ->
          und_err
            g
            d
            "class"
            (Location_1 a m)
            (\(h0, h1, h) ->
              case length h0 == length x2 of
                False -> Left ("Wrong number of arguments for class " ++ g ++ location' (Location_1 a m))
                True ->
                  let
                    repl_map = Data.Map.fromList (zip h0 x2)
                  in
                    case kindrep' repl_map h == k of
                      False -> kind_err (Location_1 a m)
                      True -> check_cats (Location_1 a m) c4 repl_map h1 *> l (Just (g, x2)))
        Nothing -> l Nothing
  type_classes ::
    (
      String ->
      Map' Kind ->
      Map' Polykind ->
      [Class_2] ->
      Map' Cat_4 ->
      (
        Map' (Class_4, Status),
        Map' (Type_2, Status),
        Map' ([String], Map' [(String, Nat)]),
        Map' (([String], [String], Kind_1), Status),
        Map' (Class_5, Status)) ->
      Err
        (
          Map' (Class_4, Status),
          Map' (Type_2, Status),
          Map' ([String], Map' [(String, Nat)]),
          Map' (([String], [String], Kind_1), Status),
          Map' (Class_5, Status)))
  type_classes a b c d w (e, g, i, o, o') =
    (
      type_classes_0 (Location_1 a) b c d w (i, o, o', Data.Map.empty) >>=
      \(r, (m, p, p', _)) -> (\(k, n) -> (n, k, m, p, p')) <$> type_classes_1 a r b (fst <$> p) (fst <$> p') w (g, e))
  type_classes_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind ->
      Map' Polykind ->
      [Class_2] ->
      Map' Cat_4 ->
      (
        Map' ([String], Map' [(String, Nat)]),
        Map' (([String], [String], Kind_1), Status),
        Map' (Class_5, Status),
        Map' String) ->
      Err
        (
          [Class_3],
          (
            Map' ([String], Map' [(String, Nat)]),
            Map' (([String], [String], Kind_1), Status),
            Map' (Class_5, Status),
            Map' String)))
  type_classes_0 a f g b m c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 a f g d m c >>= \(h, i) -> first ((:) h) <$> type_classes_0 a f g e m i
  type_classes_1 ::
    String ->
    [Class_3] ->
    Map' Kind ->
    Map' ([String], [String], Kind_1) ->
    Map' Class_5 ->
    Map' Cat_4 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_classes_1 a b u h i f c =
    case b of
      [] -> Right c
      d : e -> type_class_1 a d u h i f c >>= type_classes_1 a e u h i f
  type_constraint_0 ::
    (
      Map' (Map' [String]) ->
      Constraint_0 ->
      (Map' Kind, Map' Class_5, Map' Kind_1, Map' Cat_4) ->
      String ->
      Err (Map' (Map' [String])))
  type_constraint_0 k (Constraint_0 (Name b c) t2 (Name d e)) (u9, f, g, t5) j =
    und_err
      c
      f
      "class"
      (Location_1 j b)
      (\(Class_5 t3 m4 h y h') ->
        case length t2 == length t3 of
          False -> Left ("Incorrect number of kind arguments to class " ++ c ++ location' (Location_1 j b))
          True ->
            (
              traverse (type_kind_7 (Location_1 j) u9 Star_kind) t2 >>=
              \t' ->
                (
                  check_cats_2 (Location_1 j b) t5 (((!) (Data.Map.fromList (zip t3 t'))) <$> m4) *>
                  case Data.Map.lookup e g of
                    Just i ->
                      (
                        chain_constraints
                          (fst <$> y)
                          f
                          (Data.Map.insert
                            c
                            (Data.Map.insert
                              e
                              ((\t -> t ++ " " ++ e) <$> h')
                              (case Data.Map.lookup c k of
                                Just l -> l
                                Nothing -> Data.Map.empty))
                            k)
                          e <$
                        unif
                          (
                            "Kind mismatch in constraint " ++
                            location (Location_1 j b) ++
                            " between class " ++
                            c ++
                            " and type variable " ++
                            e ++
                            ".")
                          i
                          (fromSet (return Nothing) (Data.Set.fromList t3))
                          h)
                    Nothing -> Left ("Undefined type variable " ++ e ++ location' (Location_1 j d)))))
  type_constraints_0 ::
    (
      Map' (Map' [String]) ->
      [Constraint_0] ->
      (Map' Kind, Map' Class_5, Map' Kind_1, Map' Nat, Map' Cat_4) ->
      String ->
      Err ([Constraint_1], [String], [(String, Nat)]))
  type_constraints_0 g a (u7, f, t, u, w) h =
    case a of
      [] ->
        let
          l y = join (y <$> assocs (keys <$> g))
        in
          Right
            (
              l (\(i, j) -> Constraint_1 i [] <$> j),
              join (Data.Map.elems (join <$> Data.Map.elems <$> g)),
              l (\(i, j) -> (,) i <$> ((!) u <$> j)))
      b : c -> type_constraint_0 g b (u7, f, t, w) h >>= \d -> type_constraints_0 d c (u7, f, t, u, w) h
  type_inh :: String -> [String] -> Maybe String -> Map' String -> Either String ()
  type_inh a b c d =
    case c of
      Just e ->
        case e == a of
          False -> type_inh a (e : b) (Data.Map.lookup e d) d
          True -> Left ("Circular dependency between classes [" ++ intercalate ", " b ++ "].") 
      Nothing -> Right ()
  type_kinds_0 ::
    (Location_0 -> Location_1) -> Map' Kind -> [(String, Kind_0)] -> Map' Polykind -> Err ([(String, Kind_1)], Map' Polykind)
  type_kinds_0 a b c d =
    case c of
      [] -> Right ([], d)
      (e, f) : g ->
        type_kind_7 a b Star_kind f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a b g (Data.Map.insert e (pkind h) d)
  type_method :: (Location_0 -> Location_1) -> Method_2 -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err Method_3
  type_method a (Method_2 b c i d) e f j = type_kinds_0 a f c e >>= \(g, h) -> Method_3 b g i <$> type_typ a h f star_kind d j
  type_method_1 :: String -> (Map' Kind, Map' Class_5, Map' Cat_4) -> Method_3 -> Err Method_4
  type_method_1 e (i2, g, u) (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\(f, _, _) -> Method_4 a b f d) <$> type_constraints_0 Data.Map.empty c (i2, g, m, (\_ -> Zr) <$> m, u) e
  type_methods_0 :: (Location_0 -> Location_1) -> [Method_2] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [Method_3]
  type_methods_0 a b c d f =
    case b of
      [] -> Right []
      e : g -> type_method a e c d f >>= \h -> (:) h <$> type_methods_0 a g c d f
  type_methods_1 :: String -> (Map' Kind, Map' Class_5, Map' Cat_4) -> [Method_3] -> Err [Method_4]
  type_methods_1 e f a =
    case a of
      [] -> Right []
      b : c -> type_method_1 e f b >>= \d -> (:) d <$> type_methods_1 e f c
  unif :: String -> Kind_1 -> Map' (Maybe Kind_1) -> Kind_1 -> Err (Map' (Maybe Kind_1))
  unif a b c d =
    case (b, d) of
      (Application_kind_1 e f, Application_kind_1 g h) -> unif a e c g >>= \i -> unif a f i h
      (_, Name_kind_1 e) ->
        case Data.Map.lookup e c of
          Nothing ->
            case b == d of
              False -> Left a
              True -> Right c
          Just f ->
            case f of
              Nothing -> Right (Data.Map.insert e (Just b) c)
              Just g ->
                case b == g of
                  False -> Left a
                  True -> Right c
      _ -> Left a
--------------------------------------------------------------------------------------------------------------------------------