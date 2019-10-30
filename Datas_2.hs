--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas_2 (sysrep', type_data_br_2, type_datas_2) where
  import Data.Bifunctor
  import Data.Map
  import Datas_0
  import Datas_1
  import Standard
  import Tokenise
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
--------------------------------------------------------------------------------------------------------------------------------