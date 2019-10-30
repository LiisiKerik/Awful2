--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Datas_0 (
  Alg_pat_2 (..),
  Cat_4 (..),
  Constraint_1 (..),
  Constructor (..),
  Expression_2 (..),
  Form_2 (..),
  Kind (..),
  Kind_1 (..),
  PConstructor (..),
  Pat_1 (..),
  Polykind (..),
  Prom_alg (..),
  TPat (..),
  Type_1 (..),
  Type_2 (..),
  arrow_kind,
  arrow_type,
  check_cat,
  check_cats_2,
  comparison_type,
  constructors,
  function_type,
  hkinds,
  kind_err,
  kind_string,
  kindrep,
  kindrep',
  kinds,
  ntype,
  pconstrs,
  pkind,
  prom_algs,
  prom_type,
  promotables,
  solve_type_eqs,
  star_kind,
  type_brs_0,
  type_fields,
  type_forms,
  type_proms,
  type_rep,
  type_typ) where
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
  data Constraint_1 = Constraint_1 String [Kind_1] String deriving Show
  data Constructor = Constructor [String] [(TPat, Kind_1)] [Type_1] Type_1 [(String, Integer)] deriving Show
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
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Kind arrow") a)
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
  function_type = arrow_type star_kind
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
  hkinds =
    Data.Map.fromList
      [("Kind arrow", Arrow_kind (Arrow_kind Star_kind)), ("Nat", Star_kind), ("Ordering", Star_kind), ("Star", Star_kind)]
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
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
  kind_string :: Kind_1 -> [Kind_1] -> (String, [Kind_1])
  kind_string a b =
    case a of
      Application_kind_1 c d -> kind_string c (d : b)
      Name_kind_1 c -> (c, b)
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
  type_brs_0 :: Integer -> [String] -> [(String, Expression_2)]
  type_brs_0 a b =
    case b of
      [] -> []
      c : d -> (c, Field_expression_2 a) : type_brs_0 (a + 1) d
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
            (Application_kind_1 (Application_kind_1 (Name_kind_1 "Kind arrow") (Name_kind_1 (show i))) k)
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
      (
        Map' (Polykind, Status),
        Map' (Type_2, Status),
        Map' (Prom_alg, Status),
        Map' (Constructor, Status),
        Map' (PConstructor, Status)) ->
      Err
        (
          Map' (Polykind, Status),
          Map' (Type_2, Status),
          Map' (Prom_alg, Status),
          Map' (Constructor, Status),
          Map' (PConstructor, Status)))
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
  type_proms ::
    (
      (Location_0 -> Location_1) ->
      [Data_2] ->
      Map' Cat_4 ->
      (
        Map' Kind,
        Map' Polykind,
        Map' Expression_2,
        Map' Bool,
        Map' Type_2,
        Map' Prom_alg,
        Map' Constructor,
        Map' PConstructor) ->
      Err
        (
          (
            Map' (Kind, Status),
            Map' (Polykind, Status),
            Map' Expression_2,
            Map' Bool,
            Map' (Type_2, Status),
            Map' (Prom_alg, Status),
            Map' (Constructor, Status),
            Map' (PConstructor, Status)),
          [Data_2]))
  type_proms a b c (d, e, f, g, h, i, j, k) =
    (
      type_proms_1 a b (old d, old e, f) (make_eqs b (old (Left <$> g))) >>=
      \((l, m, n), o, p, q) ->
        (
          (\(r, s, t, u, v) -> ((l, r, n, unsafe_left <$> rem_old q, s, t, u, v), p)) <$>
          type_proms_2 a o d c (m, old h, old i, old j, old k)))
  type_proms_1 ::
    (
      (Location_0 -> Location_1) ->
      [Data_2] ->
      (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2) ->
      Map' (Either Bool (Map' Location_0), Status) ->
      Err
        (
          (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2),
          [Plain_dat],
          [Data_2],
          Map' (Either Bool (Map' Location_0), Status)))
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
      (
        Map' (Polykind, Status),
        Map' (Type_2, Status),
        Map' (Prom_alg, Status),
        Map' (Constructor, Status),
        Map' (PConstructor, Status)) ->
      Err
        (
          Map' (Polykind, Status),
          Map' (Type_2, Status),
          Map' (Prom_alg, Status),
          Map' (Constructor, Status),
          Map' (PConstructor, Status)))
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