--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Classes_0 (Class_3 (..), Class_5 (..), Method_3 (..), Nat (..), type_classes_0) where
  import Cats_0
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Set
  import Datas_0
  import Datas_1
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Class_3 = Class_3 String [String] [String] (String, Kind_1) (Maybe (Name, [Kind_1])) [Method_3] deriving Show
  data Class_5 = Class_5 [String] [String] Kind_1 (Maybe (String, [Kind_1])) [String] deriving Show
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
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
  type_methods_0 :: (Location_0 -> Location_1) -> [Method_2] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [Method_3]
  type_methods_0 a b c d f =
    case b of
      [] -> Right []
      e : g -> type_method a e c d f >>= \h -> (:) h <$> type_methods_0 a g c d f
--------------------------------------------------------------------------------------------------------------------------------