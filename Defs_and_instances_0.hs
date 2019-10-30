--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Defs_and_instances_0 (Def_4 (..), Inst (..), KT2 (..), repkinds_type, type_defs_1) where
  import Cats_0
  import Cats_1
  import Classes_0
  import Classes_1
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
  data Inst = Inst [Kind_1] [[String]] deriving Show
  data KT2 = KT2 [String] [String] [(String, Kind_1)] deriving Show
  repkinds_method :: Map' Kind_1 -> Method_4 -> Method_4
  repkinds_method a (Method_4 b c d e) = Method_4 b (second (repkinds a) <$> c) d (repkinds_type a e)
  repkinds_type :: Map' Kind_1 -> Type_1 -> Type_1
  repkinds_type a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repkinds_type a c) (repkinds_type a d)
      Name_type_1 c e d -> Name_type_1 c (repkinds a <$> e) (repkinds a <$> d)
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
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Kind arrow") l) f ->
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
  type_defs_1 ::
    (
      String ->
      Map' Kind ->
      [Def_3] ->
      Map' Polykind ->
      Map' (Type_2, Status) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err ([Def_4], Map' (Type_2, Status), (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)]))))
  type_defs_1 h x a b c y y0 v u =
    case a of
      [] -> Right ([], c, u)
      d : e ->
        type_def_1 h x d b c y y0 v u >>= \(f, g, u') -> (\(k, l, m) -> (f : k, l, m)) <$> type_defs_1 h x e b g y y0 v u'
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