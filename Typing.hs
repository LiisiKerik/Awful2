{-
todo: make a function writing operator/function. For printing stuff like "Complex (Fraction 0 1) (Fraction 1 1)"
checki abil võiks saada tüübikontrollida korraga mitut moodulit, andes ette nimekirja
ühildada Standard ja parser? või vastupidi, süntaktiline suhkur (listide sün.suhk.) standard moodulisse?
semantics of "Pair -> f" should be "Pair x y -> f x y"
mis juhtub kui esimeses moodulis on kusagil tüübimuutuja T ja järgmises moodulis sama nimega globaalne tüüp?
Let f = Crash, x = f f In 0 -- tüüpimine läheb lõpmatusse tsüklisse sest puudub occur check
"./Awful eval "List (0)"" without importing Standard.awf - error message about Writeable class looks bad; fix
let expr de-sugaring (and therefore struct name collection) completely to Standard.hs module
all de-sugaring: remove from Tree.hs, put into Standard.hs
What happens with unary minus and binary minus during parsing?
allow using operators in class method definitions? Instance Ring{Complex T}<Ring T>(..., Complex x y * Complex z w = ...)
Jaskelioff "Modular Monad Transformers" - saada need naited toole Awfulis
Scala
Lugeda tyybiperede kohta
Hargnevate andmetyypide eelised? Seosed tyybiperedega?
Klasside ja liikide vordsus - mis on seos Scala subtypinguga?
pattern matching in types
make names and types similar to Haskell
field name promotion
generalise branching types to branch not only over promoted algebraics but over Star, Int and Char
liigirakendamise eemaldamine liigituletuse kasuks (igal pool? teatud piiratud juhtudel?)
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Control.Monad
  import Data.Bifunctor
  import Data.Foldable
  import Data.List
  import Data.Map
  import Data.Set
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_2 =
    Application_alg_pat_2 String [Alg_pat_2] |
    Blank_alg_pat_2 |
    Char_alg_pat_2 Char |
    Int_alg_pat_2 Integer |
    Name_alg_pat_2 String
      deriving Show
  data Alg_pat_3 = Blank_alg_pat_3 | Char_alg_pat_3 Char | Int_alg_pat_3 Integer | Struct_alg_pat_3 String [Alg_pat_3]
    deriving Show
  data Brnch_3 = Brnch_3 String [(String, Kind_1)] String [(String, Type_8)] deriving Show
  data Class_3 = Class_3 String (String, Kind_1) (Maybe Name) [Method_3] deriving Show
  data Class_4 = Class_4 (String, Kind_1) (Maybe String) [Method_4] deriving Show
  data Class_5 = Class_5 Kind_1 (Maybe String) [String] deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  data Constructor = Constructor [(String, Kind_1)] [Type_1] Type_1 [(String, Integer)] deriving Show
  data Data_3 = Data_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Data_br_3 =
    Algebraic_data_3 [Form_1] | Branching_data_3 String [Kind_1] [Data_case_3] | Struct_data_3 String [(String, Type_8)]
      deriving Show
  data Data_case_3 = Data_case_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Def_4 =
    Basic_def_4 Location_0 String [(String, Kind_1)] [Constraint_1] Type_1 Expression_1 [String] |
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
  data Eqtns = Eqtns (Set String) [(Type_1, Type_1)] [(String, (Name, Type_1))] deriving Show
  data Expression_2 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
    Char_expression_2 Char |
    Compare_Char_0_expression_2 |
    Compare_Char_1_expression_2 Char |
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
    Name_expression_2 String |
    Negate_Int_expression_2 |
    Write_Brackets_Int_expression_2
      deriving Show
  data File =
    File
      (Map' Polykind)
      (Map' Constructor)
      (Map' Type_2)
      (Map' Kind)
      (Map' Bool)
      (Map' Class_4)
      (Map' Class_5)
      (Map' (Map' [[String]]))
      (Map' Kind_1)
      (Map' Prom_alg)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data KT2 = KT2 [String] [(String, Kind_1)] deriving Show
  data Kind = Arrow_kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_1)] [Constraint_1] Type_1 deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Pat_1 = Application_pat_1 [Pat_1] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Pattern_5 =
    Blank_pattern_5 |
    Char_blank_pattern_5 (Set Char) |
    Char_pattern_5 Char |
    Int_blank_pattern_5 (Set Integer) |
    Int_pattern_5 Integer |
    Struct_pattern_5 String [Pattern_5]
      deriving Show
  data Plain_dat = Plain_dat String [String] Plain_data_br deriving Show
  data Plain_data_br = Plain_data_alg [Form_1] | Plain_data_struct String [(String, Type_8)] deriving Show
  data Polykind = Polykind [String] Kind_1 deriving Show
  data Prom_alg = Prom_alg [String] (Map' [Kind_1]) deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Char_type_1 Char | Int_type_1 Integer | Name_type_1 String [Kind_1]
    deriving (Eq, Show)
  data Type_2 = Basic_type_1 [(String, Kind_1)] (Maybe Constraint_1) [Constraint_1] Type_1 deriving Show
  data Typedexpr =
    Application_texpr Typedexpr Typedexpr |
    Char_texpr Char |
    Function_texpr Pat_1 Typedexpr |
    Int_texpr Integer |
    Match_texpr Typedexpr [(Alg_pat_2, Typedexpr)] |
    Name_texpr_0 String String Type_1 |
    Name_texpr_1 String [(String, Type_1)]
      deriving Show
  type Types = Map' (Type_2, Status)
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
                case Data.Map.lookup g (snd (unsafe_lookup e b)) of
                  Just k -> k
                  Nothing -> [])
              (Name_expression_2 (d ++ " " ++ g))
        Name_texpr_1 d e -> addargs_2 b e (Name_expression_2 d)
  addargs_1 :: Map' ([String], Map' [(String, Nat)]) -> String -> String -> [Type_1] -> Expression_2 -> Expression_2
  addargs_1 c d e f g =
    let
      (h, i) = unsafe_lookup d c
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
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") a)
  chain_constraints :: Maybe String -> Map' Class_5 -> Map' (Map' [String]) -> String -> Map' (Map' [String])
  chain_constraints a b c e =
    case a of
      Just d ->
        let
          Class_5 _ f o = unsafe_lookup d b
        in
          chain_constraints
            f
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
  char_kind :: Kind_1
  char_kind = Name_kind_1 "Char"
  char_type :: Type_1
  char_type = ntype "Char"
  check_kind :: String -> String -> Map' (Either Polykind Kind_1) -> Type_1 -> Err Kind_1
  check_kind j c a b =
    let
      x = Left j
    in
      case b of
        Application_type_1 d e -> check_kind j c a d >>= \f -> case f of
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") g) h ->
            check_kind j c a e >>= \i -> if i == g then Right h else x
          _ -> x
        Char_type_1 _ -> Right char_kind
        Int_type_1 _ -> Right int_kind
        Name_type_1 d e ->
          if d == c
            then x
            else
              let
                (f, g) = check_kind' (unsafe_lookup d a)
              in
                Right (repkinds (Data.Map.fromList (zip f e)) g)
  check_kind' :: Either Polykind Kind_1 -> ([String], Kind_1)
  check_kind' a =
    case a of
      Left (Polykind c d) -> (c, d)
      Right b -> ([], b)
  classes_0 :: Map' Class_4
  classes_0 =
    Data.Map.fromList
      [
        (
          "Ord",
          Class_4
            ("T", star_kind)
            Nothing
            [Method_4 "Compare" [] [] (function_type (ntype "T") (function_type (ntype "T") comparison_type))]),
        (
          "Ring",
          Class_4
            ("T", star_kind)
            Nothing
            [
              Method_4 "Add" [] [] (function_type (ntype "T") (function_type (ntype "T") (ntype "T"))),
              Method_4 "Convert" [] [] (function_type int_type (ntype "T")),
              Method_4 "Multiply" [] [] (function_type (ntype "T") (function_type (ntype "T") (ntype "T"))),
              Method_4 "Negate" [] [] (function_type (ntype "T") (ntype "T"))]),
        (
          "Writeable",
          Class_4
            ("T", star_kind)
            Nothing
            [Method_4 "Write_Brackets" [] [] (function_type (ntype "T") (pair_type (list_type char_type) logical_type))])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 (_, a) b c) -> Class_5 a b ((\(Method_4 d _ _ _) -> d) <$> c)) <$> classes_0
  classes_2 :: Map' Kind_1
  classes_2 = (\(Class_4 (_, a) _ _) -> a) <$> classes_0
  comparison_kind :: Kind_1
  comparison_kind = Name_kind_1 "Comparison"
  comparison_type :: Type_1
  comparison_type = ntype "Comparison"
  compose_patterns :: (Pattern_5, Bool) -> ([Pattern_5], Bool) -> ([Pattern_5], Bool)
  compose_patterns (x, y) (z, w) = (x : z, y && w)
  constr_check :: Map' (Maybe String) -> [String] -> [[String]] -> [[String]] -> Maybe String
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
  constr_check' :: Map' (Maybe String) -> String -> [String] -> [String] -> Maybe String
  constr_check' n t x y =
    case x of 
      [] -> Nothing
      a : x' ->
        case constr_check'' n t a y of
          Left m -> Just m
          Right y' -> constr_check' n t x' y'
  constr_check'' :: Map' (Maybe String) -> String -> String -> [String] -> Err [String]
  constr_check'' m t c x =
    case x of
      [] -> Left (c ++ " " ++ t)
      a : x' -> if constr_check_3 m c a then Right x' else (:) a <$> constr_check'' m t c x'
  constr_check_3 :: Map' (Maybe String) -> String -> String -> Bool
  constr_check_3 m x y =
    if x == y
      then True
      else
        case unsafe_lookup y m of
          Just y' -> constr_check_3 m x y'
          Nothing -> False
  constructors :: Map' Constructor
  constructors =
    Data.Map.fromList
      [
        (
          "Construct_List",
          Constructor
            [("T", star_kind)]
            [ntype "T", list_type (ntype "T")]
            (list_type (ntype "T"))
            [("Construct_List", 2), ("Empty_List", 0)]),
        ("EQ", Constructor [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        ("Empty_List", Constructor [("T", star_kind)] [] (list_type (ntype "T")) [("Construct_List", 2), ("Empty_List", 0)]),
        ("False", Constructor [] [] logical_type [("False", 0), ("True", 0)]),
        ("GT", Constructor [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        ("LT", Constructor [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        (
          "Left",
          Constructor
            [("T", star_kind), ("U", star_kind)]
            [ntype "T"]
            (either_type (ntype "T") (ntype "U"))
            [("Left", 1), ("Right", 1)]),
        (
          "Mk_Pair",
          Constructor
            [("T", star_kind), ("U", star_kind)]
            [ntype "T", ntype "U"]
            (pair_type (ntype "T") (ntype "U"))
            [("Mk_Pair", 2)]),
        ("Next", Constructor [] [ntype "Nat"] (ntype "Nat") [("Next", 1), ("Zero", 0)]),
        ("Nothing", Constructor [("T", star_kind)] [] (maybe_type (ntype "T")) [("Nothing", 0), ("Wrap", 1)]),
        (
          "Right",
          Constructor
            [("T", star_kind), ("U", star_kind)]
            [ntype "U"]
            (either_type (ntype "T") (ntype "U"))
            [("Left", 1), ("Right", 1)]),
        ("True", Constructor [] [] logical_type [("False", 0), ("True", 0)]),
        ("Wrap", Constructor [("T", star_kind)] [ntype "T"] (maybe_type (ntype "T")) [("Nothing", 0), ("Wrap", 1)]),
        ("Zero", Constructor [] [] (ntype "Nat") [("Next", 1), ("Zero", 0)])]
  context_union :: (File, Map' Op) -> (File, Map' Op) -> (File, Map' Op)
  context_union (File i j d a x e q t g o, t0) (File k l h c y m r u n p, t2) =
    (
      File
        (Data.Map.union i k)
        (Data.Map.union j l)
        (Data.Map.union d h)
        (Data.Map.union a c)
        (Data.Map.union x y)
        (Data.Map.union e m)
        (Data.Map.union q r)
        (unionWith Data.Map.union t u)
        (Data.Map.union g n)
        (Data.Map.union o p),
      Data.Map.union t0 t2)
  defs :: Map' Expression_2
  defs =
    Data.Map.fromList
      [
        ("Add Int", Add_Int_0_expression_2),
        ("Compare Char", Compare_Char_0_expression_2),
        ("Compare Int", Compare_Int_0_expression_2),
        (
          "Construct_List",
          Function_expression_2
            (Name_pat_1 "x")
            (Function_expression_2
              (Name_pat_1 "y")
              (Algebraic_expression_2 "Construct_List" [Name_expression_2 "x", Name_expression_2 "y"]))),
        ("Convert Int", Convert_Int_expression_2),
        ("Div", Div_0_expression_2),
        ("EQ", Algebraic_expression_2 "EQ" []),
        ("Empty_List", Algebraic_expression_2 "Empty_List" []),
        ("False", Algebraic_expression_2 "False" []),
        ("First", Field_expression_2 0),
        ("GT", Algebraic_expression_2 "GT" []),
        ("LT", Algebraic_expression_2 "LT" []),
        ("Left", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Left" [Name_expression_2 "x"])),
        (
          "Mk_Pair",
          Function_expression_2
            (Name_pat_1 "x")
            (Function_expression_2
              (Name_pat_1 "y")
              (Algebraic_expression_2 "Mk_pair" [Name_expression_2 "x", Name_expression_2 "y"]))),
        ("Mod", Mod_0_expression_2),
        ("Multiply Int", Multiply_Int_0_expression_2),
        ("Negate Int", Negate_Int_expression_2),
        ("Next", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Next" [Name_expression_2 "x"])),
        ("Nothing", Algebraic_expression_2 "Nothing" []),
        ("Right", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Right" [Name_expression_2 "x"])),
        ("Second", Field_expression_2 1),
        ("True", Algebraic_expression_2 "True" []),
        ("Wrap", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Wrap" [Name_expression_2 "x"])),
        ("Write_Brackets Int", Write_Brackets_Int_expression_2),
        ("Zero", Algebraic_expression_2 "Zero" [])]
  either_kind :: Kind_1 -> Kind_1 -> Kind_1
  either_kind x = Application_kind_1 (Application_kind_1 (Name_kind_1 "Either") x)
  either_type :: Type_1 -> Type_1 -> Type_1
  either_type x = Application_type_1 (Application_type_1 (Name_type_1 "Either" []) x)
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (ntype "Function") a)
  gather_all_types :: (Ord u, Monad f) => (t -> Map u v -> f (Map u v)) -> [t] -> Map u v -> f (Map u v)
  gather_all_types a b c =
    case b of
      [] -> return c
      d : e -> gather_all_types a e c >>= a d
  gather_fields :: Set String -> [(String, Type_8)] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_fields b a = gather_types b (snd <$> a)
  gather_form :: Set String -> Form_1 -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_form b (Form_1 _ a) = gather_types b a
  gather_forms :: Set String -> [Form_1] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_forms a = gather_all_types (gather_form a)
  gather_type :: Set String -> Type_5 -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_type f b c =
    case b of
      Application_type_5 d e -> gather_type f e c >>= gather_type f d
      Name_type_5 (Name a d) e ->
        case e of
          [] -> Just (if Data.Set.member d f then c else Data.Map.insert d a c)
          _ -> Nothing
      _ -> Nothing
  gather_types :: Set String -> [Type_8] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_types a b = gather_all_types (gather_type a) ((\(Type_8 _ c) -> c) <$> b)
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      (Integer, Set String, [(Type_1, Type_1)], Map' Type_2) ->
      Alg_pat_1 ->
      Type_1 ->
      Err ((Integer, Set String, [(Type_1, Type_1)], Map' Type_2), (Alg_pat_2, Alg_pat_3)))
  get_pattern_type a b (d, e, f, n) g h =
    case g of
      Application_alg_pat_1 o i j ->
        und_err
          i
          b
          "constructor"
          (a o)
          (\(Constructor k x m _) ->
            let
              (q, r, s) = typevars k (d, Data.Map.empty, e)
            in
              (
                second (bimap (Application_alg_pat_2 i) (Struct_alg_pat_3 i)) <$>
                get_pattern_types a b (q, s, (h, repl' r m) : f, n) j (repl' r <$> x) (Name o i)))
      Blank_alg_pat_1 -> Right ((d, e, f, n), (Blank_alg_pat_2, Blank_alg_pat_3))
      Char_alg_pat_1 i -> Right ((d, e, (h, char_type) : f, n), (Char_alg_pat_2 i, Char_alg_pat_3 i))
      Int_alg_pat_1 i -> Right ((d, e, (h, int_type) : f, n), (Int_alg_pat_2 i, Int_alg_pat_3 i))
      Name_alg_pat_1 i ->
        Right ((d, e, f, Data.Map.insert i (Basic_type_1 [] Nothing [] h) n), (Name_alg_pat_2 i, Blank_alg_pat_3))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      (Integer, Set String, [(Type_1, Type_1)], Map' Type_2) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      Err ((Integer, Set String, [(Type_1, Type_1)], Map' Type_2), ([Alg_pat_2], [Alg_pat_3])))
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
  hkinds :: Map' Kind
  hkinds =
    Data.Map.fromList
      [
        ("Char", Star_kind),
        ("Comparison", Star_kind),
        ("Either", Arrow_kind (Arrow_kind Star_kind)),
        ("Function", Arrow_kind (Arrow_kind Star_kind)),
        ("Int", Star_kind),
        ("List", Arrow_kind Star_kind),
        ("Logical", Star_kind),
        ("Maybe", Arrow_kind Star_kind),
        ("Nat", Star_kind),
        ("Pair", Arrow_kind (Arrow_kind Star_kind)),
        ("Star", Star_kind)]
  init_type_context :: (File, Map' Op)
  init_type_context =
    (File kinds constructors types hkinds promotables classes_0 classes_1 instances classes_2 prom_algs, Data.Map.empty)
  instances :: Map' (Map' [[String]])
  instances =
    Data.Map.fromList
      [
        ("Ord", Data.Map.fromList [("Char", []), ("Int", [])]),
        ("Ring", Data.Map.fromList [("Int", [])]),
        ("Writeable", Data.Map.fromList [("Int", [])])]
  int_kind :: Kind_1
  int_kind = Name_kind_1 "Int"
  int_to_nat_type :: Integer -> Type_1
  int_to_nat_type x =
    case x of
      0 -> ntype "Zero"
      _ -> next_type (int_to_nat_type (x - 1))
  int_type :: Type_1
  int_type = ntype "Int"
  isLeft :: Either t u -> Bool
  isLeft a =
    case a of
      Left _ -> True
      Right _ -> False
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
        ("Char", Polykind [] star_kind),
        ("Comparison", Polykind [] star_kind),
        (
          "Construct_List",
          Polykind
            ["K"]
            (arrow_kind (Name_kind_1 "K") (arrow_kind (list_kind (Name_kind_1 "K")) (list_kind (Name_kind_1 "K"))))),
        ("EQ", Polykind [] comparison_kind),
        ("Either", Polykind [] (arrow_kind star_kind (arrow_kind star_kind star_kind))),
        ("Empty_List", Polykind ["K"] (list_kind (Name_kind_1 "K"))),
        ("False", Polykind [] logical_kind),
        ("Function", Polykind [] (arrow_kind star_kind (arrow_kind star_kind star_kind))),
        ("GT", Polykind [] comparison_kind),
        ("Int", Polykind [] star_kind),
        ("LT", Polykind [] comparison_kind),
        ("Left", Polykind ["K", "L"] (arrow_kind (Name_kind_1 "K") (either_kind (Name_kind_1 "K") (Name_kind_1 "L")))),
        ("List", Polykind [] (arrow_kind star_kind star_kind)),
        ("Logical", Polykind [] star_kind),
        ("Maybe", Polykind [] (arrow_kind star_kind star_kind)),
        (
          "Mk_Pair",
          Polykind
            ["K", "L"]
            (arrow_kind (Name_kind_1 "K") (arrow_kind (Name_kind_1 "L") (pair_kind (Name_kind_1 "K") (Name_kind_1 "L"))))),
        ("Nat", Polykind [] star_kind),
        ("Next", Polykind [] (arrow_kind nat_kind nat_kind)),
        ("Nothing", Polykind ["K"] (maybe_kind (Name_kind_1 "K"))),
        ("Pair", Polykind [] (arrow_kind star_kind (arrow_kind star_kind star_kind))),
        ("Right", Polykind ["K", "L"] (arrow_kind (Name_kind_1 "L") (either_kind (Name_kind_1 "K") (Name_kind_1 "L")))),
        ("True", Polykind [] logical_kind),
        ("Wrap", Polykind ["K"] (arrow_kind (Name_kind_1 "K") (maybe_kind (Name_kind_1 "K")))),
        ("Zero", Polykind [] nat_kind)]
  list_kind :: Kind_1 -> Kind_1
  list_kind = Application_kind_1 (Name_kind_1 "List")
  list_type :: Type_1 -> Type_1
  list_type = Application_type_1 (Name_type_1 "List" [])
  location_err' :: String -> Location_1 -> Location_1 -> String
  location_err' a b = location_err a (Library b)
  locations :: Locations
  locations =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        [
          "Add",
          "Char",
          "Compare",
          "Comparison",
          "Construct_List",
          "Convert",
          "Crash",
          "Div",
          "EQ",
          "Either",
          "Empty_List",
          "False",
          "First",
          "Function",
          "GT",
          "Int",
          "LT",
          "Left",
          "List",
          "Logical",
          "Maybe",
          "Mk_Pair",
          "Mod",
          "Multiply",
          "Nat",
          "Negate",
          "Next",
          "Nothing",
          "Ord",
          "Pair",
          "Right",
          "Ring",
          "Second",
          "True",
          "Wrap",
          "Write_Brackets",
          "Writeable",
          "Zero"])
  logical_kind :: Kind_1
  logical_kind = Name_kind_1 "Logical"
  logical_type :: Type_1
  logical_type = ntype "Logical"
  make_eq :: Data_2 -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eq (Data_2 a (KT1 d b) c) =
    ins_new
      a
      (case d of
        [] ->
          case promotable b (Data.Set.fromList [a]) of
            Just e ->
              case
                (case c of
                  Algebraic_data_2 f -> gather_forms e f Data.Map.empty
                  Branching_data_2 _ _ _ -> Nothing
                  Struct_data_2 _ f -> gather_fields e f Data.Map.empty) of
                    Just f -> Right f
                    Nothing -> Left False
            Nothing -> Left False
        _ -> Left False)
  make_eqs :: [Data_2] -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eqs a b =
    case a of
      [] -> b
      c : d -> make_eqs d (make_eq c b)
  maybe_kind :: Kind_1 -> Kind_1
  maybe_kind = Application_kind_1 (Name_kind_1 "Maybe")
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (Name_type_1 "Maybe" [])
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "Nat"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  next_type :: Type_1 -> Type_1
  next_type = Application_type_1 (ntype "Next")
  ntype :: String -> Type_1
  ntype a = Name_type_1 a []
  occ_kind :: String -> Kind_1 -> Bool
  occ_kind a b =
    case b of
      Application_kind_1 c d -> occ_kind a c || occ_kind a d
      Name_kind_1 c -> c == a
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  pair_type :: Type_1 -> Type_1 -> Type_1
  pair_type x = Application_type_1 (Application_type_1 (ntype "Pair") x)
  pair_kind :: Kind_1 -> Kind_1 -> Kind_1
  pair_kind x = Application_kind_1 (Application_kind_1 (Name_kind_1 "Pair") x)
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
  pkind :: Kind_1 -> Polykind
  pkind = Polykind []
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
  prom_algs :: Map' Prom_alg
  prom_algs =
    Data.Map.fromList
      [
        ("Comparison", Prom_alg [] (Data.Map.fromList [("EQ", []), ("GT", []), ("LT", [])])),
        ("Either", Prom_alg ["K", "L"] (Data.Map.fromList [("Left", [Name_kind_1 "K"]), ("Right", [Name_kind_1 "L"])])),
        (
          "List",
          Prom_alg
            ["K"]
            (Data.Map.fromList
              [
                ("Construct_List", [Name_kind_1 "K", Application_kind_1 (Name_kind_1 "List") (Name_kind_1 "K")]),
                ("Empty_List", [])])),
        ("Logical", Prom_alg [] (Data.Map.fromList [("False", []), ("True", [])])),
        ("Maybe", Prom_alg ["K"] (Data.Map.fromList [("Nothing", []), ("Wrap", [Name_kind_1 "K"])])),
        ("Nat", Prom_alg [] (Data.Map.fromList [("Next", [nat_kind]), ("Zero", [])]))]
  prom_type :: Type_1 -> Kind_1
  prom_type a =
    case a of
      Application_type_1 b c -> Application_kind_1 (prom_type b) (prom_type c)
      Name_type_1 b _ -> Name_kind_1 b
      _ -> undefined
  promotable :: [(String, Kind_0)] -> Set String -> Maybe (Set String)
  promotable a b =
    case a of
      [] -> Just b
      (c, d) : e ->
        case promotable' d of
          False -> Nothing
          True -> promotable e (Data.Set.insert c b)
  promotable' :: Kind_0 -> Bool
  promotable' (Kind_0 _ a) = a == Name_kind_0 "Star"
  promotables :: Map' Bool
  promotables =
    Data.Map.fromList
      ((\a -> (a, True)) <$> ["Char", "Comparison", "Either", "Function", "Int", "List", "Logical", "Maybe", "Nat", "Pair"])
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  repkinds :: Map' Kind_1 -> Kind_1 -> Kind_1
  repkinds a b =
    case b of
      Application_kind_1 c d -> Application_kind_1 (repkinds a c) (repkinds a d)
      Name_kind_1 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c _ ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
      _ -> b
  repl_kind_vars :: String -> [(String, Kind_1)] -> [(String, Kind_1)] -> [(String, Kind_1)]
  repl_kind_vars a b c =
    case c of
      [] -> undefined
      (d, e) : f ->
        case d == a of
          False -> (d, e) : repl_kind_vars a b f
          True -> b ++ f
  show_char :: Char -> String
  show_char c = show [c]
  slv :: Map' (Map' [[String]]) -> [(String, (Name, Type_1))] -> (Name -> String -> String -> String) -> Err ()
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
                Just j -> slv_constrs a e h g j y
                Nothing -> i
            Nothing -> i
  slv_constrs ::
    (
      Map' (Map' [[String]]) ->
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
  solve_diff :: Ord t => Map t u -> [t] -> Map t u
  solve_diff a b =
    case b of
      [] -> a
      c : d -> solve_diff (Data.Map.delete c a) d
  solve_eq ::
    (Location_0 -> Location_1) ->
    String ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err (Bool, Map' (Either Bool (Map' Location_0), Status))
  solve_eq f a b =
    case fst (unsafe_lookup a b) of
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
  solve_type_eqs :: Location_1 -> Set String -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs j a b =
    case b of
      [] -> Right ()
      d : e ->
        case d of
          (Application_kind_1 f g, Application_kind_1 h i) -> solve_type_eqs j a ((f, h) : (g, i) : e)
          (Application_kind_1 f g, Name_kind_1 h) -> solve_type_eqs' j a h (Application_kind_1 f g) e
          (Name_kind_1 f, Application_kind_1 g h) -> solve_type_eqs' j a f (Application_kind_1 g h) e
          (Name_kind_1 f, Name_kind_1 g) ->
            case f == g of
              False ->
                case Data.Set.member f a of
                  False -> solve_type_eqs' j a g (Name_kind_1 f) e
                  True -> type_reps j a f (Name_kind_1 g) e
              True -> solve_type_eqs j a e
  solve_type_eqs' :: Location_1 -> Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs' e a b c d =
    case Data.Set.member b a of
      False -> kind_err e
      True ->
        case occ_kind b c of
          False -> type_reps e a b c d
          True -> kind_err e
  solvesys ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys m b (a', t, u) =
    case b of
      [] -> Right (a', t, u)
      (c, d) : g ->
        case c of
          Application_type_1 e f ->
            case d of
              Application_type_1 h i -> solvesys m ((e, h) : (f, i) : g) (a', t, u)
              Name_type_1 h _ -> solvesys' m h c g (a', t, u)
              _ -> undefined
          Char_type_1 e ->
            case d of
              Char_type_1 f -> if e == f then solvesys m g (a', t, u) else m (show e) (show f)
              Name_type_1 f _ -> solvesys' m f c g (a', t, u)
              _ -> undefined
          Int_type_1 e ->
            case d of
              Int_type_1 f -> if e == f then solvesys m g (a', t, u) else m ('!' : show e) ('!' : show f)
              Name_type_1 f _ -> solvesys' m f c g (a', t, u)
              _ -> undefined
          Name_type_1 e _ ->
            case d of
              Name_type_1 f _ ->
                if e == f
                  then solvesys m g (a', t, u)
                  else
                    case Data.Set.member e u of
                      False ->
                        case Data.Set.member f u of
                          False -> m e f
                          True -> solvesys_rep m f c g (a', t, u)
                      True -> solvesys_rep m e d g (a', t, u)
              _ -> solvesys' m e d g (a', t, u)
  solvesys' ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys' h b c d (x, m, a) =
    let
      (y, _) = typestring c []
    in
      case Data.Set.member b a of
        False ->
          h
            b
            (case Data.Set.member y a of
              False -> y
              True -> "an application type")
        True -> solvesys_rep h b c d (x, m, a)
  solvesys_rep ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys_rep a c d e (x, f, k) =
    let
      m = sysrep' c d
    in
      solvesys a ((<$>) (bimap m m) e) (second (second m) <$> x, sysrep2 c d f, Data.Set.delete c k)
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
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      (
        ((Set String, Set String), Locations, Locations),
        (File, Map' Op),
        Map' Expression_2,
        Map' (Map' Location'),
        Map' ([String], Map' [(String, Nat)])) ->
      Err
        (
          ((Set String, Set String), Locations, Locations),
          (File, Map' Op),
          Map' Expression_2,
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)])))
  standard_naming_typing f a (b, (c, t), g, m, w) =
    (
      standard_1 (Location_1 f) t a >>=
      \(v, n') -> naming f n' b >>= \(d, e) -> (\(h, i, n, u) -> (d, (h, v), i, n, u)) <$> typing f e (c, g, m, w))
  star_kind :: Kind_1
  star_kind = Name_kind_1 "Star"
  struct_pattern :: Map' [(String, Integer)] -> String -> [(Pattern_5, Alg_pat_3)] -> ([(Pattern_5, Bool)], Bool)
  struct_pattern context x y =
    first
      (fmap (first (Struct_pattern_5 x)))
      (Prelude.foldr
        (\(z, a) -> \(b, c) -> (compose_patterns <$> z <*> b, a && c))
        ([([], True)], True)
        (uncurry (split_pattern context) <$> y))
  sysrep' :: String -> Type_1 -> Type_1 -> Type_1
  sysrep' a b c =
    let
      f = sysrep' a b
    in
      case c of
        Application_type_1 d e -> Application_type_1 (f d) (f e)
        Name_type_1 d _ -> if d == a then b else c
        _ -> c
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
      [(String, Kind_1)] ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Err (Map' (Constructor, Status), Types))
  type_branching_1 a b c (Data_case_3 d e f) g h i k l m n =
    type_data_br_2
      a
      (sysrep' c (Prelude.foldl (\o -> \(p, _) -> Application_type_1 o (ntype p)) (Name_type_1 d n) e) b)
      f
      (Prelude.foldl (\o -> \(p, q) -> Data.Map.insert p (pkind q) o) g e)
      h
      i
      (repl_kind_vars c e k)
      l
      (Prelude.foldl (\o -> \(p, q) -> Data.Map.insert p q o) m e)
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
      [(String, Kind_1)] ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Err (Map' (Constructor, Status), Types))
  type_branchings_1 a b c e f g h i j k n =
    case e of
      [] -> Right h
      l : m -> type_branching_1 a b c l f g h i j k n >>= \d -> type_branchings_1 a b c m f g d i j k n
  type_brs_0 :: Integer -> [String] -> [(String, Expression_2)]
  type_brs_0 a b =
    case b of
      [] -> []
      c : d -> (c, Field_expression_2 a) : type_brs_0 (a + 1) d
  type_case ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' Type_2 ->
      (Alg_pat_1, Expression_1) ->
      Integer ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ((Alg_pat_2, Typedexpr), Eqtns, Integer, [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a c d (Eqtns e p q) f (g, h) i j k =
    (
      get_pattern_type c a (d, e, p, f) g (ntype (show i)) >>=
      \((m, n, s, t), (o, y)) -> (\(u, v, w, r0) -> ((o, u), v, w, r0, y)) <$> type_expression a c m (Eqtns n s q) t h j k)
  type_cases ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' Type_2 ->
      [(Alg_pat_1, Expression_1)] ->
      Integer ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ([(Alg_pat_2, Typedexpr)], Eqtns, Integer, [(Location_0, [Alg_pat_3])], [Alg_pat_3]))
  type_cases b c d e f g n h i =
    case g of
      [] -> Right ([], e, d, [], [])
      l : m ->
        (
          type_case b c d e f l n h i >>=
          \(o, p, q, r0, r') -> (\(r, s, t, r1, w) -> (o : r, s, t, r0 ++ r1, r' : w)) <$> type_cases b c q p f m n h i)
  type_class_0 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Map' Polykind ->
    Class_2 ->
    (
      Map' (Map' Location'),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status),
      Map' String) ->
    Err
      (
        Class_3,
        (
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)]),
          Map' (Kind_1, Status),
          Map' (Class_5, Status),
          Map' String))
  type_class_0 a i j (Class_2 b (c, d) g' e) (m, w0, i', j0, x2) =
    (
      type_kind_7 a i Star_kind d >>=
      \h ->
        let
          g3 = (\(Name _ t4) -> t4) <$> g'
        in
          (
            type_inh b [b] g3 x2 *>
            (
              (\g ->
                let
                  g2 = (\(Method_3 w1 _ _ _) -> w1) <$> g
                in
                  (
                    Class_3 b (c, h) g' g,
                    (
                      Data.Map.insert b Data.Map.empty m,
                      Data.Map.insert b (g2, Data.Map.empty) w0,
                      ins_new b h i',
                      ins_new b (Class_5 h g3 g2) j0,
                      case g' of
                        Just (Name _ t0) -> Data.Map.insert b t0 x2
                        Nothing -> x2))) <$>
              type_methods_0 a e (Data.Map.insert c (pkind h) j) i)))
  type_class_1 ::
    String ->
    Class_3 ->
    Map' Kind_1 ->
    Map' Class_5 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_class_1 a (Class_3 b (c, k) g' e) d f1 (f0, f) =
    let
      x1 = Constraint_1 b c
      l m =
        (
          (\e' ->
            (
              Prelude.foldl
                (\x -> \(Method_4 t s u0 u) -> ins_new t (Basic_type_1 ((c, k) : s) (Just x1) (x1 : u0) u) x)
                f0
                e',
              ins_new b (Class_4 (c, k) m e') f)) <$>
          type_methods_1 a f1 e)
    in
      case g' of
        Just (Name m g) ->
          und_err g d "class" (Location_1 a m) (\h -> if h == k then l (Just g) else kind_err (Location_1 a m))
        Nothing -> l Nothing
  type_class_args ::
    Kind_1 ->
    [Pattern_0] ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat) ->
    Kind_1 ->
    Integer ->
    Type_1 ->
    Map' Kind_1 ->
    Map' Nat ->
    Nat ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat)
  type_class_args a b e g c x c0 c' n =
    case b of
      [] -> if a == g then Right ([], c, x, c0, c') else e
      h : d ->
        case a of
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") l) f ->
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
                Blank_pattern -> i (show c) (c + 1)
                Name_pattern j -> i j c
          _ -> e
  type_classes ::
    String ->
    Map' Kind ->
    Map' Polykind ->
    [Class_2] ->
    (
      Map' (Class_4, Status),
      Map' (Map' Location'),
      Map' (Type_2, Status),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status)) ->
    Err
      (
        Map' (Class_4, Status),
        Map' (Map' Location'),
        Map' (Type_2, Status),
        Map' ([String], Map' [(String, Nat)]),
        Map' (Kind_1, Status),
        Map' (Class_5, Status))
  type_classes a b c d (e, f, g, i, o, o') =
    (
      type_classes_0 (Location_1 a) b c d (f, i, o, o', Data.Map.empty) >>=
      \(r, (j, m, p, p', _)) -> (\(k, n) -> (n, j, k, m, p, p')) <$> type_classes_1 a r (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Map' Polykind ->
    [Class_2] ->
    (
      Map' (Map' Location'),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status),
      Map' String) ->
    Err
      (
        [Class_3],
        (
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)]),
          Map' (Kind_1, Status),
          Map' (Class_5, Status),
          Map' String))
  type_classes_0 a f g b c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 a f g d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 a f g e i
  type_classes_1 ::
    String ->
    [Class_3] ->
    Map' Kind_1 ->
    Map' Class_5 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_classes_1 a b h i c =
    case b of
      [] -> Right c
      d : e -> type_class_1 a d h i c >>= type_classes_1 a e h i
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
          o p = Left ("Definition " ++ i ++ location (l h) ++ " is not a component of class " ++ m ++ p)
        in
          case b of
            [] -> o "."
            (Method_4 e s y f) : g ->
              if i == e
                then (:) (p', j, s, y, f) <$> type_cls_0 a g c k l m n
                else o " or the definitions are in a wrong order."
  type_constraint_0 ::
    Map' (Map' [String]) ->
    Constraint_0 ->
    (Map' Class_5, Map' Kind_1) ->
    String ->
    Err (Map' (Map' [String]))
  type_constraint_0 k (Constraint_0 (Name b c) (Name d e)) (f, g) j =
    und_err
      c
      f
      "class"
      (Location_1 j b)
      (\(Class_5 h y h') ->
        case Data.Map.lookup e g of
          Just i ->
            if i == h
              then
                Right
                  (chain_constraints
                    y
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
                    e)
              else
                Left
                  (
                    "Kind mismatch in constraint" ++
                    location (Location_1 j b) ++
                    " between class " ++
                    c ++
                    " and type variable " ++
                    e ++
                    ".")
          Nothing -> Left ("Undefined type variable " ++ e ++ location' (Location_1 j d)))
  type_constraint_1 :: Constraint_1 -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraint_1 (Constraint_1 c e) a b =
    let
      d =
        Data.Map.insert
          c
          (Data.Map.insert
            e
            []
            (case Data.Map.lookup c a of
              Just l -> l
              Nothing -> Data.Map.empty))
          a
      Class_4 _ f _ = unsafe_lookup c b
    in
      case f of
        Just g -> type_constraint_1 (Constraint_1 g e) d b
        Nothing -> d
  type_constraints_0 ::
    Map' (Map' [String]) ->
    [Constraint_0] ->
    (Map' Class_5, Map' Kind_1, Map' Nat) ->
    String ->
    Err ([Constraint_1], [String], [(String, Nat)])
  type_constraints_0 g a (f, t, u) h =
    case a of
      [] ->
        let
          l y = join (y <$> assocs (keys <$> g))
        in
          Right
            (
              l (\(i, j) -> Constraint_1 i <$> j),
              join (Data.Map.elems (join <$> Data.Map.elems <$> g)),
              l (\(i, j) -> (,) i <$> ((\k -> unsafe_lookup k u) <$> j)))
      b : c -> type_constraint_0 g b (f, t) h >>= \d -> type_constraints_0 d c (f, t, u) h
  type_constraints_1 :: [Constraint_1] -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_data_1 ::
    (
      String ->
      (Map' Kind, Map' Prom_alg) ->
      Data_2 ->
      (Map' (Polykind, Status), Map' Expression_2) ->
      Err ((Map' (Polykind, Status), Map' Expression_2), Data_3))
-- TODO
-- implement kind polymorphism
  type_data_1 a (b, c) (Data_2 d e f) (i, k) =
    (
      type_kt_0 (Location_1 a) (e, b) >>=
      \(KT2 _ l, (_, l')) ->
        (
          (\(n, o) -> ((ins_new d (pkind (Prelude.foldr arrow_kind star_kind (snd <$> l))) i, n), Data_3 d l o)) <$>
          type_data_br_1 (a, l', c) f k))
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Types) ->
      Err (Map' (Constructor, Status), Types))
  type_data_2 a (Data_3 b c d) e f g =
    type_data_br_2
      a
      (Prelude.foldl (\n -> \n' -> Application_type_1 n (ntype n')) (ntype b) (fst <$> c))
      d
      (type_kinds c e)
      f
      g
      c
      b
      (Prelude.foldl (\m -> \(x, y) -> Data.Map.insert x y m) Data.Map.empty c)
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
      [(String, Kind_1)] ->
      String ->
      Map' Kind_1 ->
      Err (Map' (Constructor, Status), Types))
  type_data_br_2 a b c d e (f, g) l o w1 =
    let
      m i = Prelude.foldl (\j -> \(k, n) -> ins_new k (Basic_type_1 l Nothing [] n) j) g i
    in
      case c of
        Algebraic_data_3 i ->
          (
            (\j ->
              (
                Prelude.foldl
                  (\k -> \(Form_2 n p) ->
                    ins_new n (Constructor l p b ((\(Form_2 h y) -> (h, fromIntegral (length y))) <$> j)) k)
                  f
                  j,
                m ((\(Form_2 k n) -> (k, Prelude.foldr function_type b n)) <$> j))) <$>
            type_forms a i d e)
        Branching_data_3 j h k -> type_branchings_1 a b j k (Data.Map.delete j d) e (f, g) l o (Data.Map.delete j w1) h
        Struct_data_3 i j ->
          (
            (\k ->
              (
                ins_new i (Constructor l (snd <$> k) b [(i, fromIntegral (length j))]) f,
                m ((i, Prelude.foldr function_type b (snd <$> k)) : (second (function_type b) <$> k)))) <$>
            type_fields a j d e)
  type_datas ::
    (
      String ->
      [Data_2] ->
      (
        Map' (Polykind, Status),
        Map' (Constructor, Status),
        Types,
        Map' (Kind, Status),
        Map' Expression_2,
        Map' (Bool, Status),
        Map' (Prom_alg, Status)) ->
      Err
        (
          Map' (Polykind, Status),
          Map' (Constructor, Status),
          Types,
          Map' (Kind, Status),
          Map' Expression_2,
          Map' (Bool, Status),
          Map' (Prom_alg, Status)))
  type_datas h a (b, i, d, o, c, x, a0) =
    (
      type_proms_1 (Location_1 h) a (o, b, c) (make_eqs a (first Left <$> x)) >>=
      \((e, p, r), f, g, k) ->
        (
          type_proms_2 (Location_1 h) f (fst <$> o) (p, d, a0, i) >>=
          \(t, n, b0, l) ->
            (
              type_datas_1 h (fst <$> e, fst <$> b0) g (t, r) >>=
              \((u, w), y) ->
                (
                  (\(b', c') -> (u, b', c', e, w, first unsafe_left <$> k, b0)) <$>
                  type_datas_2 (Location_1 h) y (fst <$> u) (fst <$> e) (l, n)))))
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
      (Map' (Constructor, Status), Types) ->
      Err (Map' (Constructor, Status), Types))
  type_datas_2 f a b y c =
    case a of
      [] -> Right c
      d : e -> type_data_2 f d b y c >>= type_datas_2 f e b y
  type_def_1 ::
    String ->
    Map' Kind ->
    Def_3 ->
    Map' Polykind ->
    Map' (Type_2, Status) ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err
      (
        Def_4,
        Map' (Type_2, Status),
        Map' (Map' Location'),
        Map' (Map' ([[String]], Status)),
        Map' ([String], Map' [(String, Nat)]))
  type_def_1 l x a b c k k2 t t' u3 =
    case a of
      Basic_def_3 f d e e' g i ->
        (
          type_kinds_1 (Location_1 l) x e b Data.Map.empty >>=
          \(y, j, j') ->
            (
              type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') l >>=
                \(o1, o2, _) ->
                (
                  (\h -> (Basic_def_4 f d y o1 h i o2, ins_new d (Basic_type_1 y Nothing o1 h) c, t, t', u3)) <$>
                  type_typ (Location_1 l) g j x star_kind)))
      Instance_3 d (Name e m) (Name f n) w2 k' o' g ->
        und_err
          m
          k
          "class"
          (Location_1 l e)
          (\(Class_4 (o, p) w0 q) ->
            und_err
              n
              b
              "type"
              (Location_1 l f)
              (\(Polykind r s) ->
                (
                  ziphelp (Location_1 l) x n f Data.Map.empty r w2 >>=
                    \(e4, w3) ->
                      (
                        type_class_args
                          (repkinds w3 s)
                          k'
                          (kind_err (Location_1 l f))
                          p
                          0
                          (Name_type_1 n e4)
                          Data.Map.empty
                          Data.Map.empty
                          Zr >>=
                        \(q', p', s', t0, t7) ->
                          (
                            type_constraints_0 Data.Map.empty o' (k2, t0, t7) l >>=
                            \(o1, o2, o3) ->
                              let
                                r' =
                                  (
                                    (\(x', _) ->
                                      (
                                        (\(Constraint_1 y' _) -> y') <$>
                                        Data.List.filter (\(Constraint_1 _ y') -> y' == x') o1)) <$>
                                    q')
                              in
                                (
                                  type_cls_0 n q s' g (Location_1 l) m d >>=
                                  \w ->
                                    case Data.Map.lookup n (unsafe_lookup m t) of
                                      Just u -> Left (location_err ("instances of " ++ m ++ " " ++ n) u (Location_1 l d))
                                      Nothing ->
                                        Right
                                          (
                                            Instance_4 d m w0 o n q' p' w s' o1 o2 r',
                                            c,
                                            adjust (Data.Map.insert n (Library (Location_1 l d))) m t,
                                            (case Data.Map.lookup m t' of
                                              Just _ -> adjust (ins_new n r') m
                                              Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t',
                                            adjust (second (Data.Map.insert n o3)) m u3)))))))
  type_def_2 ::
    (Location_0 -> Location_1) ->
    Def_4 ->
    (Map' Constructor, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Kind ->
    Err (Map' Expression_2)
  type_def_2 j a d c m n t' u0 v2 =
    case a of
      Basic_def_4 r e b x h i y' ->
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
            (Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z (pkind w) y) n b, v2))
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
                      Just u ->
                        case constr_check ((\(Class_4 _ q0 _) -> q0) <$> u0) (fst <$> e0) u r' of
                          Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                          Nothing -> r
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
  type_defs ::
    String ->
    Map' Kind ->
    [Def_3] ->
    [Name] ->
    (Map' Polykind, Map' Constructor) ->
    (Map' Expression_2, Types) ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err
      (
        Map' Expression_2,
        Types,
        Map' (Map' Location'),
        Map' (Map' ([[String]], Status)),
        Map' ([String], Map' [(String, Nat)]))
  type_defs h x a a2 (b, i) (c, d) y y0 z t u' =
    (
      type_defs_1 h x a b d y y0 z t u' >>=
      \(g, e, k, u, f') ->
        (
          (\f -> (f, e, k, u, f')) <$
          type_ops h (fst <$> e) a2 <*>
          type_defs_2 (Location_1 h) g (i, fst <$> e) c ((<$>) fst <$> u) b f' y x))
  type_defs_1 ::
    String ->
    Map' Kind ->
    [Def_3] ->
    Map' Polykind ->
    Types ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err ([Def_4], Types, Map' (Map' Location'), Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)]))
  type_defs_1 h x a b c y y0 z u v =
    case a of
      [] -> Right ([], c, z, u, v)
      d : e ->
        (
          type_def_1 h x d b c y y0 z u v >>=
          \(f, g, t, u', v') -> (\(k, l, m, t', k') -> (f : k, l, m, t', k')) <$> type_defs_1 h x e b g y y0 t u' v')
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' Constructor, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Kind ->
    Err (Map' Expression_2)
  type_defs_2 f a b c g i j u v =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i j u v >>= \h -> type_defs_2 f e b h g i j u v
  type_eqs ::
    (
      (Location_0 -> Location_1) ->
      Integer ->
      Type_5 ->
      Map' Polykind ->
      Map' Kind ->
      Kind_1 ->
      (Set String, [(Kind_1, Kind_1)]) ->
      Err (Integer, (Set String, [(Kind_1, Kind_1)]), Type_1))
  type_eqs m i a d e k (s, f) =
    case a of
      Application_type_5 b c ->
        (
          type_eqs
            m
            (i + 1)
            b
            d
            e
            (Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") (Name_kind_1 (show i))) k)
            (Data.Set.insert (show i) s, f) >>=
          \(i', f', b') ->
            (\(i2, f2, c') -> (i2, f2, Application_type_1 b' c')) <$> type_eqs m i' c d e (Name_kind_1 (show i)) f')
      Char_type_5 b -> Right (i, (s, (k, char_kind) : f), Char_type_1 b)
      Int_type_5 b -> Right (i, (s, (k, int_kind) : f), Int_type_1 b)
      Name_type_5 (Name l b) c ->
        und_err
          b
          d
          "type"
          (m l)
          (\(Polykind p q) ->
            (\(t, u) -> (i, (s, (k, repkinds u q) : f), Name_type_1 b t)) <$> ziphelp m e b l Data.Map.empty p c)
  type_expr ::
    String ->
    Type_1 ->
    (Location_0 -> Location_1) ->
    (Map' Constructor, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Integer ->
    Map' ([String], Map' [(String, Nat)]) ->
    (Map' Polykind, Map' Kind) ->
    Err Expression_2
  type_expr k h a (c, e) f m w w' b =
    let
      n = " in " ++ k
    in
      (
        type_expression c a w (Eqtns Data.Set.empty [] []) e f h b >>=
        \(g, (Eqtns i j x), _, x3) ->
          (
            solvesys (\y -> \p -> Left ("Type mismatch between " ++ min y p ++ " and " ++ max y p ++ n)) j (x, g, i) >>=
            \(y, p, k') ->
              case Data.Set.null k' of
                False -> Left ("Unresolved type variables" ++ n)
                True ->
                  (
                    addargs w' p <$
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
                    pats a ((\(Constructor _ _ _ e') -> e') <$> c) x3)))
  type_expr' ::
    (Map' Polykind, Map' Constructor, Map' Type_2, Map' Kind) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err Expression_2
  type_expr' (b, c, e, i) f g h =
    type_expr
      "input."
      (list_type char_type)
      (Location_1 "input")
      (c, e)
      (Application_expression_1
        (Name_expression_1 (Name (Location_0 0 0) "First") Nothing [])
        (Application_expression_1 (Name_expression_1 (Name (Location_0 0 0) "Write_Brackets") Nothing []) f))
      g
      0
      h
      (b, i)
  type_expression ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' Type_2 ->
      Expression_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err (Typedexpr, Eqtns, Integer, [(Location_0, [Alg_pat_3])]))
  type_expression v r o (Eqtns f h c') d b e (r7, m8) =
    let
      x' a = location' (r a)
    in
      case b of
        Application_expression_1 c g ->
          (
            type_expression
              v
              r
              (o + 1)
              (Eqtns (Data.Set.insert (show o) f) h c')
              d
              c
              (function_type (ntype (show o)) e)
              (r7, m8) >>=
            \(i, j, p, d7) ->
              (
                (\(l, m, q, e') -> (Application_texpr i l, m, q, d7 ++ e')) <$>
                type_expression v r p j d g (ntype (show o)) (r7, m8)))
        Char_expression_1 c -> Right (Char_texpr c, Eqtns f ((e, char_type) : h) c', o, [])
        Function_expression_1 c g ->
          (
            type_pat r v c (ntype (show o)) d (o + 1) (Data.Set.insert (show o) f) h >>=
            \(a6, b6, c6, d6, f6) ->
              (
                (\(a', b', d', f4) -> (Function_texpr a6 a', b', d', f4)) <$>
                type_expression
                  v
                  r
                  (c6 + 1)
                  (Eqtns (Data.Set.insert (show c6) d6) ((e, function_type (ntype (show o)) (ntype (show c6))) : f6) c')
                  b6
                  g
                  (ntype (show c6))
                  (r7, m8)))
        Int_expression_1 c -> Right (Int_texpr c, Eqtns f ((e, int_type) : h) c', o, [])
        Match_expression_1 a7 c g ->
          (
            type_expression v r (o + 1) (Eqtns (Data.Set.insert (show o) f) h c') d c (ntype (show o)) (r7, m8) >>=
            \(k, m, n, n2) ->
              (\(q, u, x, n3, n4) -> (Match_texpr k q, u, x, n2 ++ [(a7, n4)] ++ n3)) <$> type_cases v r n m d g o e (r7, m8))
        Name_expression_1 (Name a7 c) g k ->
          let
            e5 a' = Left ("Too " ++ a' ++ " type arguments for variable " ++ c ++ x' a7)
          in
            und_err
              c
              d
              "variable"
              (r a7)
              (\(Basic_type_1 i x0 a' j) ->
                let
                  g7 k2 d3 e4 s' r' =
                    (
                      (\(s, p, n) ->
                        let
                          x7 = (\(Constraint_1 a0 b0) -> (a0, (Name a7 c, unsafe_lookup b0 p))) <$> a'
                        in
                          (k2 x7, Eqtns n ((e, repl' p j) : h) (x7 ++ c'), s, [])) <$>
                      case k of
                        [] -> Right (typevars d3 (s', e4, r'))
                        _ -> (\f9 -> (s', f9, r')) <$> typevars' r (r7, m8) e5 d3 k e4)
                in
                  case g of
                    Just t3 ->
                      case x0 of
                        Just (Constraint_1 y0 _) ->
                          case i of
                            [] -> undefined
                            (d5, k3) : d' ->
                              (
                                type_typ r t3 r7 m8 k3 >>=
                                \t7 ->
                                  g7
                                    (\_ -> Name_texpr_0 c y0 t7)
                                    d'
                                    (Data.Map.singleton d5 t7)
                                    o
                                    f)
                        Nothing -> Left ("Invalid class argument for variable " ++ c ++ x' a7)
                    Nothing ->
                      case x0 of
                        Just (Constraint_1 y0 _) ->
                          case i of
                            [] -> undefined
                            (d5, _) : d' ->
                              g7
                                (\_ -> Name_texpr_0 c y0 (Name_type_1 (show o) []))
                                d'
                                (Data.Map.singleton d5 (Name_type_1 (show o) []))
                                (o + 1)
                                (Data.Set.insert (show o) f)
                        Nothing -> g7 (\t9 -> Name_texpr_1 c (second snd <$> t9)) i Data.Map.empty o f)
  type_exprs ::
    (
      (Name -> String) ->
      (Location_0 -> Location_1) ->
      (Map' Constructor, Map' Type_2) ->
      Map' (Map' [[String]]) ->
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)] ->
      (Map' Expression_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      (Expression_2 -> Expression_2) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      (Map' Polykind, Map' Kind) ->
      Err (Map' Expression_2))
  type_exprs a b c d h i t z w f' t' t0 (x2, t4) =
    case h of
      [] -> Right i
      (j @ (Name _ y), k, s, t5, l) : m ->
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
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' (pkind m0) k') x2 s, t4) >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (f' g) i) t z w f' t' t0 (x2, t4))
  type_field :: (Location_0 -> Location_1) -> (String, Type_8) -> Map' Polykind -> Map' Kind -> Err (String, Type_1)
  type_field d (a, b) c e  = (,) a <$> type_typ d b c e star_kind
  type_fields :: (Location_0 -> Location_1) -> [(String, Type_8)] -> Map' Polykind -> Map' Kind -> Err [(String, Type_1)]
  type_fields f a b g =
    case a of
      [] -> Right []
      c : d -> type_field f c b g >>= \e -> (:) e <$> type_fields f d b g
  type_form :: (Location_0 -> Location_1) -> Form_1 -> Map' Polykind -> Map' Kind -> Err Form_2
  type_form d (Form_1 a b) c e = Form_2 a <$> type_types d b c e
  type_forms :: (Location_0 -> Location_1) -> [Form_1] -> Map' Polykind -> Map' Kind -> Err [Form_2]
  type_forms f a b g =
    case a of
      [] -> Right []
      c : d -> type_form f c b g >>= \e -> (:) e <$> type_forms f d b g
  type_inh :: String -> [String] -> Maybe String -> Map' String -> Either String ()
  type_inh a b c d =
    case c of
      Just e ->
        case e == a of
          False -> type_inh a (e : b) (Data.Map.lookup e d) d
          True -> Left ("Circular dependency between classes [" ++ intercalate ", " b ++ "].") 
      Nothing -> Right ()
  type_kind :: (String, Kind_1) -> Map' Polykind -> Map' Polykind
  type_kind (a, b) = Data.Map.insert a (pkind b)
  type_kind_4 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    (String, Kind_0) ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kind_4 d e (g, a) b =
    (
      (\h ->
        let
          f = Data.Map.insert g h
        in
          bimap f f b) <$>
      type_kind_7 d e Star_kind a)
  type_kind_6 :: (Location_0 -> Location_1) -> Map' Kind -> Kind_0 -> Err (Kind_1, Kind)
  type_kind_6 a b (Kind_0 c d) =
    case d of
      Application_kind_0 e f ->
        (
          type_kind_6 a b e >>=
          \(g, h) ->
            case h of
              Arrow_kind j -> (\k -> (Application_kind_1 g k, j)) <$> type_kind_7 a b Star_kind f
              Star_kind -> kind_err (a c))
      Name_kind_0 e -> und_err e b "kind" (a c) (\f -> Right (Name_kind_1 e, f))
  type_kind_7 :: (Location_0 -> Location_1) -> Map' Kind -> Kind -> Kind_0 -> Err Kind_1
  type_kind_7 a b c (Kind_0 d e) =
    case e of
      Application_kind_0 f g ->
        (
          type_kind_6 a b f >>=
          \(h, i) ->
            case i of
              Arrow_kind k -> if k == c then Application_kind_1 h <$> type_kind_7 a b Star_kind g else kind_err (a d)
              Star_kind -> kind_err (a d))
      Name_kind_0 f -> und_err f b "kind" (a d) (\g -> if g == c then Right (Name_kind_1 f) else kind_err (a d))
  type_kinds :: [(String, Kind_1)] -> Map' Polykind -> Map' Polykind
  type_kinds a b =
    case a of
      [] -> b
      c : d -> type_kinds d (type_kind c b)
  type_kinds_0 ::
    (Location_0 -> Location_1) -> Map' Kind -> [(String, Kind_0)] -> Map' Polykind -> Err ([(String, Kind_1)], Map' Polykind)
  type_kinds_0 a b c d =
    case c of
      [] -> Right ([], d)
      (e, f) : g ->
        type_kind_7 a b Star_kind f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a b g (Data.Map.insert e (pkind h) d)
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
  type_kinds_4 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [(String, Kind_0)] ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kinds_4 e f a b =
    case a of
      [] -> Right b
      c : d -> type_kind_4 e f c b >>= type_kinds_4 e f d
  type_kinds_5 ::
    (Location_0 -> Location_1) -> Map' Kind -> ([(String, Kind_0)], Map' Kind_1) -> Err ([(String, Kind_1)], Map' Kind_1)
  type_kinds_5 f a (b, x) =
    case b of
      [] -> Right ([], x)
      (g, c) : d -> type_kind_7 f a Star_kind c >>= \e -> first ((:) (g, e)) <$> type_kinds_5 f a (d, Data.Map.insert g e x)
  type_kinds_9 :: (Location_0 -> Location_1) -> Map' Kind -> [String] -> [Kind_0] -> (String -> String) -> Err [Kind_1]
  type_kinds_9 a b c d i =
    case c of
      [] ->
        case d of
          [] -> Right []
          _ : _ -> Left (i "many")
      _ : e ->
        case d of
          [] -> Left (i "few")
          f : g -> type_kind_7 a b Star_kind f >>= \h -> (:) h <$> type_kinds_9 a b e g i
  type_kt_0 :: (Location_0 -> Location_1) -> (KT1, Map' Kind) -> Err (KT2, (Map' Kind, Map' Kind_1))
-- TODO: finish implementing this part here
-- this is actually not kind-polymorphic
-- this code assumes that there are no kind variables
  type_kt_0 a (KT1 b c, d) = bimap (KT2 b) ((,) d) <$> type_kinds_5 a d (c, Data.Map.empty)
  type_method :: (Location_0 -> Location_1) -> Method_2 -> Map' Polykind -> Map' Kind -> Err Method_3
  type_method a (Method_2 b c i d) e f = type_kinds_0 a f c e >>= \(g, h) -> Method_3 b g i <$> type_typ a d h f star_kind
  type_method_1 :: String -> Map' Class_5 -> Method_3 -> Err Method_4
  type_method_1 e g (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\(f, _, _) -> Method_4 a b f d) <$> type_constraints_0 Data.Map.empty c (g, m, (\_ -> Zr) <$> m) e
  type_methods_0 :: (Location_0 -> Location_1) -> [Method_2] -> Map' Polykind -> Map' Kind -> Err [Method_3]
  type_methods_0 a b c d =
    case b of
      [] -> Right []
      e : g -> type_method a e c d >>= \h -> (:) h <$> type_methods_0 a g c d
  type_methods_1 :: String -> Map' Class_5 -> [Method_3] -> Err [Method_4]
  type_methods_1 e f a =
    case a of
      [] -> Right []
      b : c -> type_method_1 e f b >>= \d -> (:) d <$> type_methods_1 e f c
  type_ops :: String -> Map' Type_2 -> [Name] -> Err ()
  type_ops a b c =
    case c of
      [] -> Right ()
      Name d e : f ->
        und_err
          e
          b
          "function"
          (Location_1 a d)
          (\(Basic_type_1 _ _ _ g) ->
            case g of
              Application_type_1
                (Application_type_1 (Name_type_1 "Function" []) _)
                (Application_type_1 (Application_type_1 (Name_type_1 "Function" []) _) _) ->
                  type_ops a b f
              _ -> Left ("Function " ++ e ++ location (Location_1 a d) ++ " takes less than 2 arguments."))
  type_pat ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      Pat ->
      Type_1 ->
      Map' Type_2 ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err (Pat_1, Map' Type_2, Integer, Set String, [(Type_1, Type_1)]))
  type_pat k h (Pat g b) c d l n o =
    case b of
      Application_pat e f ->
        und_err
        e
        h
        "constructor"
        (k g)
        (\(Constructor i j m a) ->
          case a of
            [_] ->
              let
                (p, q, r) = typevars i (l, Data.Map.empty, n)
              in
                (
                  (\(s, t, u, v, w) -> (Application_pat_1 s, t, u, v, w)) <$>
                  type_pats k h f (repl' q <$> j) d p r ((c, repl' q m) : o) (Name g e))
            _ -> Left ("Constructor " ++ e ++ location (k g) ++ " is not a struct constructor."))
      Blank_pat -> Right (Blank_pat_1, d, l, n, o)
      Name_pat e -> Right (Name_pat_1 e, Data.Map.insert e (Basic_type_1 [] Nothing [] c) d, l, n, o)
  type_pats ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      [Pat] ->
      [Type_1] ->
      Map' Type_2 ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Name ->
      Err ([Pat_1], Map' Type_2, Integer, Set String, [(Type_1, Type_1)]))
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
  type_prom_1 ::
    (Location_0 -> Location_1) ->
    Data_2 ->
    (Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        Maybe ((Map' (Kind, Status), Map' (Polykind, Status), Map' Expression_2), Plain_dat),
        Map' (Either Bool (Map' Location_0), Status))
  type_prom_1 a (Data_2 b (KT1 y c) d) (e, f, h) k =
    case y of
      [] ->
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
      _ -> Right (Nothing, k)
  type_prom_2 ::
    (
      (Location_0 -> Location_1) ->
      Plain_dat ->
      Map' Kind ->
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status)))
  type_prom_2 f (Plain_dat a b c) y' (d, e, a0, k7) =
    let
      g = Prelude.foldl (\n -> \y -> Data.Map.insert y (pkind star_kind) n) (fst <$> d) b
      x = Prelude.foldl (\n -> \y -> Application_type_1 n (ntype y)) (ntype a) b
      g0 = (\t -> (t, star_kind)) <$> b
      promhelp p' q' =
        ins_new
          p'
          (Polykind
            b
            (Prelude.foldr
              (\x' -> arrow_kind (prom_type x'))
              (Prelude.foldl (\t' -> \u' -> Application_kind_1 t' (Name_kind_1 u')) (Name_kind_1 a) b)
              q'))
      b' = Basic_type_1 g0 Nothing []
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
                    ins_new u (Constructor g0 v x ((\(Form_2 r s) -> (r, fromIntegral (length s))) <$> q)) t)
                  k7
                  q)) <$>
            type_forms f h g y')
        Plain_data_struct m3 h ->
          (
            (\i ->
              (
                promhelp m3 (snd <$> i) d,
                Prelude.foldl
                  (flip (\(k, l) -> ins_new k (b' (function_type x l))))
                  (ins_new m3 (b' (Prelude.foldr (function_type <$> snd) x i)) e)
                  i,
                a0,
                ins_new m3 (Constructor g0 (snd <$> i) x [(m3, fromIntegral (length i))]) k7)) <$>
            type_fields f h g y')
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
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status)))
  type_proms_2 a b y d =
    case b of
      [] -> Right d
      e : f -> type_prom_2 a e y d >>= type_proms_2 a f y
  type_reps :: Location_1 -> Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> Err ()
  type_reps e a b c d =
    let
      f = kindrep b c
    in
      solve_type_eqs e (Data.Set.delete b a) (bimap f f <$> d)
  type_typ :: (Location_0 -> Location_1) -> Type_8 -> Map' Polykind -> Map' Kind -> Kind_1 -> Err Type_1
  type_typ a (Type_8 b c) d e f = type_eqs a 0 c d e f (Data.Set.empty, []) >>= \(_, (g, h), i) -> i <$ solve_type_eqs (a b) g h
{-
  type_type :: (Location_0 -> Location_1) -> Location_0 -> Type_5 -> Map' Polykind -> Map' Kind -> Kind_1 -> Err Type_1
  type_type l a c d y e =
    let
      x = kind_err (l a)
    in
      case c of
        Application_type_5 f g ->
          (
            type_type' l a f d y >>=
            \(h, i) ->
              case i of
                Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") j) k ->
                  if k == e then Application_type_1 h <$> type_type l a g d y j else x
                _ -> x)
        Char_type_5 b -> if e == char_kind then Right (Char_type_1 b) else x
        Int_type_5 b -> if e == int_kind then Right (Int_type_1 b) else x
        Name_type_5 (Name a' f) b ->
          und_err
            f
            d
            "type"
            (l a')
            (\(Polykind h g) ->
              ziphelp l y f a' Data.Map.empty h b >>= \(z, w) -> if repkinds w g == e then Right (Name_type_1 f z) else x)
  type_type' :: (Location_0 -> Location_1) -> Location_0 -> Type_5 -> Map' Polykind -> Map' Kind -> Err (Type_1, Kind_1)
  type_type' l a c d y =
    case c of
      Application_type_5 e f ->
        (
          type_type' l a e d y >>=
          \(g, h) ->
            case h of
              Application_kind_1 (Application_kind_1 (Name_kind_1 "Function") i) j ->
                (\k -> (Application_type_1 g k, j)) <$> type_type l a f d y i
              _ -> kind_err (l a))
      Char_type_5 e -> Right (Char_type_1 e, char_kind)
      Int_type_5 e -> Right (Int_type_1 e, int_kind)
      Name_type_5 (Name a' e) g ->
        und_err
          e
          d
          "type"
          (l a')
          (\(Polykind h f) -> bimap (Name_type_1 e) (\w -> repkinds w f) <$> ziphelp l y e a' Data.Map.empty h g)
-}
  type_types :: (Location_0 -> Location_1) -> [Type_8] -> Map' Polykind -> Map' Kind -> Err [Type_1]
  type_types f a b g =
    case a of
      [] -> Right []
      c : d -> type_typ f c b g star_kind >>= \e -> (:) e <$> type_types f d b g
  type_types' :: (Location_0 -> Location_1) -> (Map' Kind, Map' Polykind) -> [(String, Type_8)] -> Err [(String, Type_1)]
  type_types' a (b, c) d =
    case d of
      [] -> Right []
      (e, f) : g -> type_typ a f c b star_kind >>= \h -> (:) (e, h) <$> type_types' a (b, c) g
  types :: Map' Type_2
  types =
    Data.Map.fromList
      [
        (
          "Add",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))),
        (
          "Compare",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ord" "T"))
            [Constraint_1 "Ord" "T"]
            (function_type (ntype "T") (function_type (ntype "T") comparison_type))),
        (
          "Construct_List",
          Basic_type_1
            [("T", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (function_type (list_type (ntype "T")) (list_type (ntype "T"))))),
        (
          "Convert",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type int_type (ntype "T"))),
        ("Crash", Basic_type_1 [("T", star_kind)] Nothing [] (ntype "T")),
        ("Div", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        ("EQ", Basic_type_1 [] Nothing [] comparison_type),
        ("Empty_List", Basic_type_1 [("T", star_kind)] Nothing [] (list_type (ntype "T"))),
        ("False", Basic_type_1 [] Nothing [] logical_type),
        (
          "First",
          Basic_type_1
            [("T", star_kind), ("U", star_kind)]
            Nothing
            []
            (function_type (pair_type (ntype "T") (ntype "U")) (ntype "T"))),
        ("GT", Basic_type_1 [] Nothing [] comparison_type),
        (
          "Left",
          Basic_type_1
            [("T", star_kind), ("U", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (either_type (ntype "T") (ntype "U")))),
        ("LT", Basic_type_1 [] Nothing [] comparison_type),
        (
          "Mk_Pair",
          Basic_type_1
            [("T", star_kind), ("U", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (function_type (ntype "U") (pair_type (ntype "T") (ntype "U"))))),
        ("Mod", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        (
          "Multiply",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))),
        (
          "Negate",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (ntype "T") (ntype "T"))),
        ("Next", Basic_type_1 [] Nothing [] (function_type nat_type nat_type)),
        ("Nothing", Basic_type_1 [("T", star_kind)] Nothing [] (maybe_type (ntype "T"))),
        (
          "Right",
          Basic_type_1
            [("T", star_kind), ("U", star_kind)]
            Nothing
            []
            (function_type (ntype "U") (either_type (ntype "T") (ntype "U")))),
        (
          "Second",
          Basic_type_1
            [("T", star_kind), ("U", star_kind)]
            Nothing
            []
            (function_type (pair_type (ntype "T") (ntype "U")) (ntype "U"))),
        ("True", Basic_type_1 [] Nothing [] logical_type),
        ("Wrap", Basic_type_1 [("T", star_kind)] Nothing [] (function_type (ntype "T") (maybe_type (ntype "T")))),
        (
          "Write_Brackets",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Writeable" "T"))
            [Constraint_1 "Writeable" "T"]
            (function_type (ntype "T") (pair_type (list_type char_type) logical_type))),
        ("Zero", Basic_type_1 [] Nothing [] nat_type)]
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Char_type_1 b -> (show b, d)
      Int_type_1 b -> ('!' : show b, d)
      Name_type_1 b _ -> (b, d)
  typevar :: String -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevar a (c, e, f) =
    let
      d = show c
    in
      (c + 1, Data.Map.insert a (Name_type_1 d []) e, Data.Set.insert d f)
  typevars :: [(String, Kind_1)] -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevars a b =
    case a of
      [] -> b
      (c, _) : d -> typevars d (typevar c b)
  typevars' ::
    (
      (Location_0 -> Location_1) ->
      (Map' Polykind, Map' Kind) ->
      (String -> Err (Map' Type_1)) ->
      [(String, Kind_1)] ->
      [Type_8] ->
      Map' Type_1 ->
      Err (Map' Type_1))
  typevars' l (j, k) a b c d =
    case b of
      [] ->
        case c of
          [] -> Right d
          _ -> a "many"
      (e, f) : g ->
        case c of
          [] -> a "few"
          h : i -> type_typ l h j k f >>= \m -> typevars' l (j, k) a g i (Data.Map.insert e m d)
  typing ::
    String ->
    Tree_5 ->
    (File, Map' Expression_2, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)])) ->
    Err (File, Map' Expression_2, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)]))
  typing k (Tree_5 a a' x7 c) (File d t v w w0 b' c5 x t2 u0, l, m', n4) =
    (
      type_datas k a (old d, old t, old v, old w, l, old w0, old u0) >>=
      \(e, b, g, o, f, w1, u1) ->
        (
          type_classes k (fst <$> o) (fst <$> e) a' (old b', m', g, n4, old t2, old c5) >>=
          \(c', m2, g0, x1, x2, t3) ->
            (
              (\(i, j, n', y, y2) ->
                (
                  File
                    (rem_old e)
                    (rem_old b)
                    (rem_old j)
                    (rem_old o)
                    (rem_old w1)
                    (rem_old c')
                    (rem_old t3)
                    (rem_old' y)
                    (rem_old x2)
                    (rem_old u1),
                  i,
                  n',
                  y2)) <$>
              type_defs k (fst <$> o) c x7 (fst <$> e, fst <$> b) (f, g0) (fst <$> c') (fst <$> t3) m2 (old' x) x1)))
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
  unsafe_lookup :: Ord t => t -> Map t u -> u
  unsafe_lookup a b =
    case Data.Map.lookup a b of
      Just c -> c
      Nothing -> undefined
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