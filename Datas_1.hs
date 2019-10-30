--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas_1 (Data_3 (..), Data_br_3 (..), Data_case_3 (..), type_data_br_1, type_datas_1, type_kind_7) where
  import Data.Bifunctor
  import Data.Map
  import Datas_0
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Data_3 = Data_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Data_br_3 =
    Algebraic_data_3 [Form_1] | Branching_data_3 String [Kind_1] [Data_case_3] | Struct_data_3 String [(String, Type_8)]
      deriving Show
  data Data_case_3 = Data_case_3 String [(String, Kind_1)] Data_br_3 deriving Show
  type_branching_0 ::
    (
      (String, Map' Kind_1, Map' Prom_alg, String) ->
      Data_case_2 ->
      (Map' (Either Location_0 [Kind_1]), Map' Expression_2) ->
      Err ((Map' (Either Location_0 [Kind_1]), Map' Expression_2), Data_case_3))
  type_branching_0 (a, b, c, m) (Data_case_2 (Name e f) g h) (i, j) =
    case Data.Map.lookup f i of
      Nothing -> Left (show f ++ location (Location_1 a e) ++ " is not a type constructor of kind " ++ m ++ ".")
      Just l ->
        case l of
          Left k ->
            Left ("Conflicting cases for " ++ show f ++ " in " ++ a ++ " at " ++ location0 k ++ " and " ++ location0 e ++ ".")
          Right k ->
            case compare (length k) (length g) of
              LT -> Left ("Type constructor " ++ show f ++ location (Location_1 a e) ++ " has been given too few arguments.")
              EQ ->
                let
                  p = zip g k
                in
                  (
                    (\(n, o) -> ((insert f (Left e) i, n), Data_case_3 f p o)) <$>
                    type_data_br_1 (a, Prelude.foldl (\q -> \(r, s) -> insert r s q) b p, c) h j)
              GT -> Left ("Type constructor " ++ show f ++ location (Location_1 a e) ++ " has been given too many arguments.")
  type_branchings_0 ::
    (
      (String, Map' Kind_1, Map' Prom_alg, Map' (Either Location_0 [Kind_1]), String) ->
      [Data_case_2] ->
      Map' Expression_2 ->
      Err (Map' Expression_2, [Data_case_3]))
  type_branchings_0 (a, b, c, d, m) e f =
    case e of
      [] -> Right (f, [])
      h : i ->
        type_branching_0 (a, b, c, m) h (d, f) >>= \((j, k), l) -> second ((:) l) <$> type_branchings_0 (a, b, c, j, m) i k
  type_data_1 ::
    (
      String ->
      (Map' Kind, Map' Prom_alg) ->
      Data_2 ->
      (Map' (Polykind, Status), Map' Expression_2) ->
      Err ((Map' (Polykind, Status), Map' Expression_2), Data_3))
  type_data_1 a (b, c) (Data_2 d m4 f) (i, k) =
    (
      type_kinds_5 (Location_1 a) b (m4, empty) >>=
      \(l, y) ->
        (
          (\(n, o) ->
            ((ins_new d (Polykind Nothing [] (Prelude.foldr arrow_kind star_kind (snd <$> l))) i, n), Data_3 d l o)) <$>
          type_data_br_1 (a, y, c) f k))
  type_data_br_1 :: (String, Map' Kind_1, Map' Prom_alg) -> Data_br_2 -> Map' Expression_2 -> Err (Map' Expression_2, Data_br_3)
  type_data_br_1 (a, l, k) b c =
    case b of
      Algebraic_data_2 e ->
        Right
          (
            Prelude.foldl
              (\d -> \(Form_1 f g) ->
                let
                  j = show <$> [0 .. length g - 1]
                in
                  insert
                    f
                    (Prelude.foldr
                      (\h -> Function_expression_2 (Name_pat_1 h))
                      (Algebraic_expression_2 f (Name_expression_2 <$> j))
                      j)
                    d)
              c
              e,
            Algebraic_data_3 e)
      Branching_data_2 _ (Name e f) g ->
        und_err
          f
          l
          "type variable"
          (Location_1 a e)
          (\h ->
            let
              (m, n) = kind_string h []
            in
              case Data.Map.lookup m k of
                Nothing -> Left ("Type variable " ++ f ++ location (Location_1 a e) ++ " is not branchable.")
                Just (Prom_alg i j) ->
                  (
                    second (Branching_data_3 f n) <$>
                    type_branchings_0 (a, delete f l, k, (\t -> Right (fmap (kindrep' (fromList (zip i n))) t)) <$> j, m) g c))
      Struct_data_2 e f ->
        let
          d = (\(g, _) -> '!' : g) <$> f
        in
          Right
            (
              Prelude.foldl
                (\g -> \(h, i) -> insert h i g)
                (insert
                  e
                  (Prelude.foldr
                    (\h -> Function_expression_2 (Name_pat_1 h))
                    (Algebraic_expression_2 e (Name_expression_2 <$> d))
                    d)
                  c)
                (type_brs_0 0 (fst <$> f)),
              Struct_data_3 e f)
  type_datas_1 ::
    (
      String ->
      (Map' Kind, Map' Prom_alg) ->
      [Data_2] ->
      (Map' (Polykind, Status), Map' Expression_2) ->
      Err ((Map' (Polykind, Status), Map' Expression_2), [Data_3]))
  type_datas_1 f a b c =
    case b of
      [] -> Right (c, [])
      d : e -> type_data_1 f a d c >>= \(g, h) -> second ((:) h) <$> type_datas_1 f a e g
  type_kind_6 :: (Location_0 -> Location_1) -> Map' Kind -> Kind_0 -> Err (Kind_1, Kind)
  type_kind_6 a b d =
    case d of
      Application_kind_0 e f ->
        (
          type_kind_6 a b e >>=
          \(g, h) ->
            case h of
              Arrow_kind j -> (\k -> (Application_kind_1 g k, j)) <$> type_kind_7 a b Star_kind f
-- TODO: ADD ERROR LOCATION
              Star_kind -> Left "Kind error.")
      Name_kind_0 (Name c e) -> und_err e b "kind" (a c) (\f -> Right (Name_kind_1 e, f))
  type_kind_7 :: (Location_0 -> Location_1) -> Map' Kind -> Kind -> Kind_0 -> Err Kind_1
  type_kind_7 a b c e =
    case e of
      Application_kind_0 f g ->
        (
          type_kind_6 a b f >>=
          \(h, i) ->
            case i of
-- TODO: ADD ERROR LOCATION
              Arrow_kind k -> if k == c then Application_kind_1 h <$> type_kind_7 a b Star_kind g else Left "Kind error."
              Star_kind -> Left "Kind error.")
      Name_kind_0 (Name d f) -> und_err f b "kind" (a d) (\g -> if g == c then Right (Name_kind_1 f) else kind_err (a d))
  type_kinds_5 ::
    (Location_0 -> Location_1) -> Map' Kind -> ([(String, Kind_0)], Map' Kind_1) -> Err ([(String, Kind_1)], Map' Kind_1)
  type_kinds_5 f a (b, x) =
    case b of
      [] -> Right ([], x)
      (g, c) : d -> type_kind_7 f a Star_kind c >>= \e -> first ((:) (g, e)) <$> type_kinds_5 f a (d, insert g e x)
--------------------------------------------------------------------------------------------------------------------------------