--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Classes_1 (Class_4 (..), Method_4 (..), check_cats, type_classes_1, type_constraints_0) where
  import Classes_0
  import Control.Monad
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Datas
  import Standard
  import Tokenise
  import Tree
  data Class_4 = Class_4 [String] [String] (String, Kind_1) (Maybe (String, [Kind_1])) [Method_4] deriving Show
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
  type_method_1 :: String -> (Map' Kind, Map' Class_5, Map' Cat_4) -> Method_3 -> Err Method_4
  type_method_1 e (i2, g, u) (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\(f, _, _) -> Method_4 a b f d) <$> type_constraints_0 Data.Map.empty c (i2, g, m, (\_ -> Zr) <$> m, u) e
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