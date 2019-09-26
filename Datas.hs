--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas where
  import Data.Bifunctor
  import Data.Functor
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_2 = Application_alg_pat_2 String [Alg_pat_2] | Blank_alg_pat_2 | Int_alg_pat_2 Integer | Name_alg_pat_2 String
    deriving Show
  data Cat_4 = Cat_4 [String] [String] deriving Show
  data Cat_5 =
    Cat_5
      Location_0
      (String, [String])
      [String]
      ((Location_0, Patn'), (Location_0, Patn'), Data_br_2, Expression_1, Expression_1)
        deriving Show
  data Cat_6 = Cat_6 Location_0 (String, [String]) [String] (Expression_1, Expression_1) deriving Show
  data Constraint_1 = Constraint_1 String [Kind_1] String deriving Show
  data Constructor = Constructor [String] [(TPat, Kind_1)] [Type_1] Type_1 [(String, Integer)] deriving Show
  data Data_3 = Data_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Data_br_3 =
    Algebraic_data_3 [Form_1] | Branching_data_3 String [Kind_1] [Data_case_3] | Struct_data_3 String [(String, Type_8)]
      deriving Show
  data Data_case_3 = Data_case_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Expression_2 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
    Compare_Int_0_expression_2 |
    Compare_Int_1_expression_2 Integer |
    Convert_Int_expression_2 |
    Div_0_expression_2 |
    Div_1_expression_2 Integer |
    Field_expression_2 Integer |
    Function_expression_2 Pat_1 Expression_2 |
    Int_expression_2 Integer |
    Match_expression_2 Expression_2 [(Alg_pat_2, Expression_2)] |
    Mod_0_expression_2 |
    Mod_1_expression_2 Integer |
    Multiply_Int_0_expression_2 |
    Multiply_Int_1_expression_2 Integer |
    Name_expression_2 String
      deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Kind = Arrow_kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
  data PConstructor = PConstructor [String] [Kind_1] Kind_1 [(String, Integer)] deriving Show
  data Pat_1 = Application_pat_1 [Pat_1] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Plain_dat = Plain_dat String [String] Plain_data_br deriving Show
  data Plain_data_br = Plain_data_alg [Form_1] | Plain_data_struct String [(String, Type_8)] deriving Show
  data Polykind = Polykind (Maybe String) [String] Kind_1 deriving Show
  data Prom_alg = Prom_alg [String] (Map' [Kind_1]) deriving Show
  data TPat = Application_tpat String [TPat] | Name_tpat String deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String (Maybe Kind_1) [Kind_1] deriving (Eq, Show)
  data Type_2 = Type_2 (Maybe String) [String] [String] [(TPat, Kind_1)] (Maybe Constraint_1) [Constraint_1] Type_1
    deriving Show
  type Types = Map' (Type_2, Status)
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") a)
  arrow_type :: Kind_1 -> Type_1 -> Type_1 -> Type_1
  arrow_type k a = Application_type_1 (Application_type_1 (Name_type_1 "Arrow" (Just k) []) a)
  check_cat :: Location_1 -> Map' Cat_4 -> Kind_1 -> Err ()
  check_cat g a b =
    let
      (c, d) = kind_string b []
    in
      case Data.Map.lookup c a of
        Nothing -> Left ("Kind " ++ c ++ " should be an instance of Cat because of something going on" ++ location' g)
        Just (Cat_4 e f) -> check_cats' g a (zip d e) (Data.Set.fromList f)
  check_cats' :: Location_1 -> Map' Cat_4 -> [(Kind_1, String)] -> Set String -> Err ()
  check_cats' a b c d =
    case c of
      [] -> Right ()
      (e, f) : g ->
        (
          (case Data.Set.member f d of
            False -> Right ()
            True -> check_cat a b e) *>
          check_cats' a b g d)
  check_cats_2 :: Location_1 -> Map' Cat_4 -> [Kind_1] -> Err ()
  check_cats_2 a b c =
    case c of
      [] -> Right ()
      d : e -> check_cat a b d *> check_cats_2 a b e
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
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Arrow" (Just (Name_kind_1 "Star")) []) a)
  gather_all_types :: Ord u => (t -> Map u v -> Map u v) -> [t] -> Map u v -> Map u v
  gather_all_types a b c =
    case b of
      [] -> c
      d : e -> a d (gather_all_types a e c)
  gather_fields :: Set String -> [(String, Type_8)] -> Map' Location_0 -> Map' Location_0
  gather_fields b a = gather_types b (snd <$> a)
  gather_form :: Set String -> Form_1 -> Map' Location_0 -> Map' Location_0
  gather_form b (Form_1 _ a) = gather_types b a
  gather_forms :: Set String -> [Form_1] -> Map' Location_0 -> Map' Location_0
  gather_forms a = gather_all_types (gather_form a)
  gather_type :: Set String -> Type_5 -> Map' Location_0 -> Map' Location_0
  gather_type f b c =
    case b of
      Application_type_5 d e -> gather_type f d (gather_type f e c)
      Name_type_5 (Name a d) -> if Data.Set.member d f then c else Data.Map.insert d a c
  gather_types :: Set String -> [Type_8] -> Map' Location_0 -> Map' Location_0
  gather_types a b = gather_all_types (gather_type a) ((\(Type_8 _ c) -> c) <$> b)
  hkinds :: Map' Kind
  hkinds = Data.Map.fromList [("Nat", Star_kind), ("Ordering", Star_kind), ("Star", Star_kind)]
  int_type :: Type_1
  int_type = ntype "Int"
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
  kind_string :: Kind_1 -> [Kind_1] -> (String, [Kind_1])
  kind_string a b =
    case a of
      Application_kind_1 c d -> kind_string c (d : b)
      Name_kind_1 c -> (c, b)
  kindrep :: String -> Kind_1 -> Kind_1 -> Kind_1
  kindrep a b f =
    let
      e = kindrep a b
    in
      case f of
        Application_kind_1 c d -> Application_kind_1 (e c) (e d)
        Name_kind_1 c ->
          case c == a of
            False -> f
            True -> b
  kindrep' :: Map' Kind_1 -> Kind_1 -> Kind_1
  kindrep' a b =
    let
      c = kindrep' a
    in
      case b of
        Application_kind_1 d e -> Application_kind_1 (c d) (c e)
        Name_kind_1 d ->
          case Data.Map.lookup d a of
            Nothing -> b
            Just e -> e
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
  make_eq :: Data_2 -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eq (Data_2 a b c) =
    ins_new
      a
      (case promotable b (Data.Set.fromList [a]) of
        Just e ->
          case c of
            Algebraic_data_2 f -> Right (gather_forms e f Data.Map.empty)
            Branching_data_2 _ _ _ -> Left False
            Struct_data_2 _ f -> Right (gather_fields e f Data.Map.empty)
        Nothing -> Left False)
  make_eqs :: [Data_2] -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eqs a b =
    case a of
      [] -> b
      c : d -> make_eqs d (make_eq c b)
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "Nat"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  ntype :: String -> Type_1
  ntype a = Name_type_1 a Nothing []
  occ_kind :: String -> Kind_1 -> Bool
  occ_kind a b =
    case b of
      Application_kind_1 c d -> occ_kind a c || occ_kind a d
      Name_kind_1 c -> c == a
  pconstrs :: Map' PConstructor
  pconstrs =
    (
      (\(Constructor _ a b c d) -> PConstructor ((\(Name_tpat e, _) -> e) <$> a) (prom_type <$> b) (prom_type c) d) <$>
      constructors)
  pkind :: Kind_1 -> Polykind
  pkind = Polykind Nothing []
  prom_algs :: Map' Prom_alg
  prom_algs =
    Data.Map.fromList
      [
        ("Nat", Prom_alg [] (Data.Map.fromList [("Next", [nat_kind]), ("Zero", [])])),
        ("Ordering", Prom_alg [] (Data.Map.fromList [("EQ", []), ("GT", []), ("LT", [])]))]
  prom_type :: Type_1 -> Kind_1
  prom_type a =
    case a of
      Application_type_1 b c -> Application_kind_1 (prom_type b) (prom_type c)
      Name_type_1 b _ _ -> Name_kind_1 b
  promotable :: [(String, Kind_0)] -> Set String -> Maybe (Set String)
  promotable a b =
    case a of
      [] -> Just b
      (c, d) : e ->
        case promotable' d of
          False -> Nothing
          True -> promotable e (Data.Set.insert c b)
  promotable' :: Kind_0 -> Bool
  promotable' a =
    case a of
      Name_kind_0 (Name _ "Star") -> True
      _ -> False
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
  solve_diff :: Ord t => Map t u -> [t] -> Map t u
  solve_diff a b =
    case b of
      [] -> a
      c : d -> solve_diff (Data.Map.delete c a) d
  solve_eq ::
    (
      (Location_0 -> Location_1) ->
      String ->
      Map' (Either Bool (Map' Location_0), Status) ->
      Err (Bool, Map' (Either Bool (Map' Location_0), Status)))
  solve_eq f a b =
    case fst (b ! a) of
      Left d -> Right (d, b)
      Right d -> (\e -> (e, ins_new a (Left e) b)) <$> solve_eq_help f [a] b d
  solve_eq_help ::
    (Location_0 -> Location_1) -> [String] -> Map' (Either Bool (Map' Location_0), Status) -> Map' Location_0 -> Err Bool
  solve_eq_help h a b c =
    case minViewWithKey c of
      Just ((d, e), f) ->
        und_err
          d
          b
          "type"
          (h e)
          (\g ->
            case fst g of
              Left i -> Right i
              Right i -> solve_eq_help h (d : a) b (Data.Map.union f (solve_diff i a)))
      Nothing -> Right True
  solve_type_eqs ::
    Location_1 -> Set String -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1, [Kind_1]) -> Err (Map' Kind_1, Type_1, [Kind_1])
  solve_type_eqs j a b k =
    case b of
      [] ->
        case Data.Set.null a of
          False -> Left ("Unresolved kind variables " ++ location' j)
          True -> Right k
      d : e ->
        case d of
          (Application_kind_1 f g, Application_kind_1 h i) -> solve_type_eqs j a ((f, h) : (g, i) : e) k
          (Application_kind_1 f g, Name_kind_1 h) -> solve_type_eqs' j a h (Application_kind_1 f g) e k
          (Name_kind_1 f, Application_kind_1 g h) -> solve_type_eqs' j a f (Application_kind_1 g h) e k
          (Name_kind_1 f, Name_kind_1 g) ->
            case f == g of
              False ->
                case Data.Set.member f a of
                  False -> solve_type_eqs' j a g (Name_kind_1 f) e k
                  True -> type_reps j a f (Name_kind_1 g) e k
              True -> solve_type_eqs j a e k
  solve_type_eqs' ::
    (
      Location_1 ->
      Set String ->
      String ->
      Kind_1 ->
      [(Kind_1, Kind_1)] ->
      (Map' Kind_1, Type_1, [Kind_1]) ->
      Err (Map' Kind_1, Type_1, [Kind_1]))
  solve_type_eqs' e a b c d k =
    case Data.Set.member b a of
      False -> kind_err e
      True ->
        case occ_kind b c of
          False -> type_reps e a b c d k
          True -> kind_err e
  star_kind :: Kind_1
  star_kind = Name_kind_1 "Star"
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
                    (\(n, o) -> ((Data.Map.insert f (Left e) i, n), Data_case_3 f p o)) <$>
                    type_data_br_1 (a, Prelude.foldl (\q -> \(r, s) -> Data.Map.insert r s q) b p, c) h j)
              GT -> Left ("Type constructor " ++ show f ++ location (Location_1 a e) ++ " has been given too many arguments.")
  type_branching_1 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      String ->
      Data_case_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Types) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Types))
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
  type_branchings_1 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      String ->
      [Data_case_3] ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Types) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Types))
  type_branchings_1 a b c e f g h i j k n n7 =
    case e of
      [] -> Right h
      l : m -> type_branching_1 a b c l f g h i j k n n7 >>= \d -> type_branchings_1 a b c m f g d i j k n n7
  type_brs_0 :: Integer -> [String] -> [(String, Expression_2)]
  type_brs_0 a b =
    case b of
      [] -> []
      c : d -> (c, Field_expression_2 a) : type_brs_0 (a + 1) d
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
  type_data_1 ::
    (
      String ->
      (Map' Kind, Map' Prom_alg) ->
      Data_2 ->
      (Map' (Polykind, Status), Map' Expression_2) ->
      Err ((Map' (Polykind, Status), Map' Expression_2), Data_3))
  type_data_1 a (b, c) (Data_2 d m4 f) (i, k) =
    (
      type_kinds_5 (Location_1 a) b (m4, Data.Map.empty){-type_kt_0 (Location_1 a) b m4-} >>=
      \(l, y) ->
        (
          (\(n, o) ->
            ((ins_new d (Polykind Nothing [] (Prelude.foldr arrow_kind star_kind (snd <$> l))) i, n), Data_3 d l o)) <$>
          type_data_br_1 (a, y, c) f k))
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_3 ->
      Map' Polykind ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Constructor, Status), Types) ->
      Err (Map' (Constructor, Status), Types))
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
                  Data.Map.insert
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
                    type_branchings_0
                      (a, Data.Map.delete f l, k, (\t -> Right (fmap (kindrep' (Data.Map.fromList (zip i n))) t)) <$> j, m)
                      g
                      c))
      Struct_data_2 e f ->
        let
          d = (\(g, _) -> '!' : g) <$> f
        in
          Right
            (
              Prelude.foldl
                (\g -> \(h, i) -> Data.Map.insert h i g)
                (Data.Map.insert
                  e
                  (Prelude.foldr
                    (\h -> Function_expression_2 (Name_pat_1 h))
                    (Algebraic_expression_2 e (Name_expression_2 <$> d))
                    d)
                  c)
                (type_brs_0 0 (fst <$> f)),
              Struct_data_3 e f)
  type_data_br_2 ::
    (
      (Location_0 -> Location_1) ->
      Type_1 ->
      Data_br_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Types) ->
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      Map' Cat_4 ->
      Err (Map' (Constructor, Status), Types))
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
      type_proms_1 (Location_1 a) b (old j, old g, s) (make_eqs b (first Left <$> old k)) >>=
      \((u, v, w), x, y, z) ->
        (
          type_proms_2 (Location_1 a) x j q (v, old i, old p, old h, old r) >>=
          \(a', b', c', d', e') ->
            (
              (,) <$> type_datas_1 a (fst <$> u, fst <$> c') y (a', w) <*> type_cats_0 (Location_1 a, fst <$> u) c (old q) >>=
              \(((f', g'), h'), (i', j')) ->
                (
                  type_datas_2 (Location_1 a) h' (fst <$> f') (fst <$> u) (fst <$> i') (d', b') >>=
                  \(k', l') ->
                    (
                      type_cats_1 (a, fst <$> u, fst <$> e', fst <$> f', fst <$> c', fst <$> i') j' (k', l', g') >>=
                      \((m', n', o'), p') -> Right ((f', m', n', u, unsafe_left <$> rem_old z, c', i', e', o'), p'))))))
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
  type_datas_2 ::
    (
      (Location_0 -> Location_1) ->
      [Data_3] ->
      Map' Polykind ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Constructor, Status), Types) ->
      Err (Map' (Constructor, Status), Types))
  type_datas_2 f a b y c' c =
    case a of
      [] -> Right c
      d : e -> type_data_2 f d b y c' c >>= type_datas_2 f e b y c'
  type_eqs ::
    (
      (Location_0 -> Location_1) ->
      Integer ->
      Type_5 ->
      Map' Polykind ->
      Map' Kind ->
      Kind_1 ->
      (Set String, [(Kind_1, Kind_1)], [Kind_1]) ->
      Err (Integer, (Set String, [(Kind_1, Kind_1)], [Kind_1]), Type_1))
  type_eqs m i a d e k (s, f, u4) =
    case a of
      Application_type_5 b c ->
        (
          type_eqs
            m
            (i + 1)
            b
            d
            e
            (Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") (Name_kind_1 (show i))) k)
            (Data.Set.insert (show i) s, f, u4) >>=
          \(i', f', b') ->
            (\(i2, f2, c') -> (i2, f2, Application_type_1 b' c')) <$> type_eqs m i' c d e (Name_kind_1 (show i)) f')
      Name_type_5 (Name b c) ->
        und_err
          c
          d
          "type"
          (m b)
          (\(Polykind j l n) ->
            let
              o = maybeToList j ++ l
              p = i + fromIntegral (length o)
              q = show <$> [i .. p - 1]
              x = Name_kind_1 <$> q
              (m1, n1) =
                case j of
                  Nothing -> ([], Name_type_1 c Nothing x)
                  Just _ -> ([head x], Name_type_1 c (Just (head x)) (tail x))
            in
              Right
                (
                  p,
                  (Data.Set.union s (Data.Set.fromList q), [(k, kindrep' (Data.Map.fromList (zip o x)) n)] ++ f, m1 ++ u4),
                  n1))
  type_field ::
    (Location_0 -> Location_1) -> (String, Type_8) -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err (String, Type_1)
  type_field d (a, b) c e f  = (,) a <$> type_typ d c e star_kind b f
  type_fields ::
    (Location_0 -> Location_1) -> [(String, Type_8)] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [(String, Type_1)]
  type_fields f a b g h =
    case a of
      [] -> Right []
      c : d -> type_field f c b g h >>= \e -> (:) e <$> type_fields f d b g h
  type_form :: (Location_0 -> Location_1) -> Form_1 -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err Form_2
  type_form d (Form_1 a b) c e f = Form_2 a <$> type_types d b c e f
  type_forms :: (Location_0 -> Location_1) -> [Form_1] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [Form_2]
  type_forms f a b g h =
    case a of
      [] -> Right []
      c : d -> type_form f c b g h >>= \e -> (:) e <$> type_forms f d b g h
  type_kind :: (String, Kind_1) -> Map' Polykind -> Map' Polykind
  type_kind (a, b) = Data.Map.insert a (pkind b)
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
  type_kinds :: [(String, Kind_1)] -> Map' Polykind -> Map' Polykind
  type_kinds a b =
    case a of
      [] -> b
      c : d -> type_kinds d (type_kind c b)
  type_kinds_5 ::
    (Location_0 -> Location_1) -> Map' Kind -> ([(String, Kind_0)], Map' Kind_1) -> Err ([(String, Kind_1)], Map' Kind_1)
  type_kinds_5 f a (b, x) =
    case b of
      [] -> Right ([], x)
      (g, c) : d -> type_kind_7 f a Star_kind c >>= \e -> first ((:) (g, e)) <$> type_kinds_5 f a (d, Data.Map.insert g e x)
  type_prom_1 ::
    (Location_0 -> Location_1) ->
    Data_2 ->
    (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        Maybe ((Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2), Plain_dat),
        Map' (Either Bool (Map' Location_0), Status))
  type_prom_1 a (Data_2 b c d) (e, f, h) k =
    let
      n p j =
        (
          first
            (\i ->
              case i of
                False -> Nothing
                True ->
                  Just
                    (
                      (
                        ins_new b (Prelude.foldr (return Arrow_kind) Star_kind c) e,
                        ins_new b (pkind (Prelude.foldr arrow_kind star_kind (return star_kind <$> c))) f,
                        Prelude.foldl (\m -> \(o, q) -> Data.Map.insert o q m) h p),
                      Plain_dat b (fst <$> c) j)) <$>
          solve_eq a b k)
    in
      case d of
        Algebraic_data_2 j ->
          n
            (
              (\(Form_1 i p) ->
                let
                  p2 = show <$> findIndices (return True) p
                in
                  (
                    i,
                    Prelude.foldr
                      (\q1 -> Function_expression_2 (Name_pat_1 q1))
                      (Algebraic_expression_2 i (Name_expression_2 <$> p2))
                      p2)) <$>
              j)
            (Plain_data_alg j)
        Branching_data_2 _ _ _ -> Right (Nothing, k)
        Struct_data_2 j l ->
          let
            v = fst <$> l
            w = (:) '!' <$> v
          in
            n
              (
                (
                  j,
                  Prelude.foldr
                    (\p -> Function_expression_2 (Name_pat_1 ('!' : p)))
                    (Algebraic_expression_2 j (Name_expression_2 <$> w))
                    w) :
                type_brs_0 0 v)
              (Plain_data_struct j l)
  type_prom_2 ::
    (
      (Location_0 -> Location_1) ->
      Plain_dat ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)))
  type_prom_2 f (Plain_dat a b c) y' y7 (d, e, a0, k7, t9) =
    let
      g = Prelude.foldl (\n -> \y -> Data.Map.insert y (pkind star_kind) n) (fst <$> d) b
      x = Prelude.foldl (\n -> \y -> Application_type_1 n (ntype y)) (ntype a) b
      promhelp p' q' =
        ins_new
          p'
          (Polykind
            Nothing
            b
            (Prelude.foldr
              (\x' -> arrow_kind (prom_type x'))
              (Prelude.foldl (\t' -> \u' -> Application_kind_1 t' (Name_kind_1 u')) (Name_kind_1 a) b)
              q'))
      g2 = (\t -> (Name_tpat t, star_kind)) <$> b
      b' = Type_2 Nothing [] [] g2 Nothing []
    in
      case c of
        Plain_data_alg h ->
          (
            (\q ->
              (
                Prelude.foldl (\t -> \(Form_2 u v) -> promhelp u v t) d q,
                Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (b' (Prelude.foldr function_type x m)))) e q,
                ins_new a (Prom_alg b (Data.Map.fromList ((\(Form_2 r s) -> (r, prom_type <$> s)) <$> q))) a0,
                Prelude.foldl
                  (\t -> \(Form_2 u v) ->
                    ins_new u (Constructor [] g2 v x ((\(Form_2 r s) -> (r, fromIntegral (length s))) <$> q)) t)
                  k7
                  q,
                Prelude.foldl
                  (\t -> \(Form_2 u v) ->
                    ins_new
                      u
                      (PConstructor b (prom_type <$> v) (prom_type x) ((\(Form_2 r s) -> (r, fromIntegral (length s))) <$> q))
                      t)
                  t9
                  q)) <$>
            type_forms f h g y' y7)
        Plain_data_struct m3 h ->
          (
            (\i ->
              (
                promhelp m3 (snd <$> i) d,
                Prelude.foldl
                  (flip (\(k, l) -> ins_new k (b' (function_type x l))))
                  (ins_new m3 (b' (Prelude.foldr (function_type <$> snd) x i)) e)
                  i,
                ins_new a (Prom_alg b (Data.Map.singleton m3 (prom_type <$> snd <$> i))) a0,
                ins_new m3 (Constructor [] g2 (snd <$> i) x [(m3, fromIntegral (length i))]) k7,
                ins_new m3 (PConstructor b (prom_type <$> snd <$> i) (prom_type x) [(m3, fromIntegral (length i))]) t9)) <$>
            type_fields f h g y' y7)
  type_proms_1 ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2),
        [Plain_dat],
        [Data_2],
        Map' (Either Bool (Map' Location_0), Status))
  type_proms_1 a b c f =
    case b of
      [] -> Right (c, [], [], f)
      d : e -> type_prom_1 a d c f >>= \(h, g) ->
        let
          o p q = p <$> type_proms_1 a e q g
        in
          case h of
            Just (i, j) -> o (\(k, l, m, n) -> (k, j : l, m, n)) i
            Nothing -> o (\(k, l, m, n) -> (k, l, d : m, n)) c
  type_proms_2 ::
    (
      (Location_0 -> Location_1) ->
      [Plain_dat] ->
      Map' Kind ->
      Map' Cat_4 ->
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)))
  type_proms_2 a b y c d =
    case b of
      [] -> Right d
      e : f -> type_prom_2 a e y c d >>= type_proms_2 a f y c
  type_rep :: (Kind_1 -> Kind_1) -> Type_1 -> Type_1
  type_rep a c =
    case c of
      Application_type_1 d e -> Application_type_1 (type_rep a d) (type_rep a e)
      Name_type_1 d e f -> Name_type_1 d (a <$> e) (a <$> f)
  type_reps ::
    (
      Location_1 ->
      Set String ->
      String ->
      Kind_1 ->
      [(Kind_1, Kind_1)] ->
      (Map' Kind_1, Type_1, [Kind_1]) ->
      Err (Map' Kind_1, Type_1, [Kind_1]))
  type_reps e a b c d (k, l, u) =
    let
      f = kindrep b c
    in
      solve_type_eqs e (Data.Set.delete b a) (bimap f f <$> d) (f <$> k, type_rep f l, f <$> u)
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
  type_typ :: (Location_0 -> Location_1) -> Map' Polykind -> Map' Kind -> Kind_1 -> Type_8 -> Map' Cat_4 -> Err Type_1
  type_typ a d e f (Type_8 b c) j =
    (
      type_eqs a 0 c d e f (Data.Set.empty, [], []) >>=
      \(_, (g, h, t), i) -> solve_type_eqs (a b) g h (Data.Map.empty, i, t) >>= \(_, y, m) -> check_cats_2 (a b) j m $> y)
  type_types :: (Location_0 -> Location_1) -> [Type_8] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [Type_1]
  type_types f a b g h =
    case a of
      [] -> Right []
      c : d -> type_typ f b g star_kind c h >>= \e -> (:) e <$> type_types f d b g h
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
--------------------------------------------------------------------------------------------------------------------------------