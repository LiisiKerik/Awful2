{-
What happens with unary minus and binary minus during parsing?
Jaskelioff "Modular Monad Transformers" - saada need naited toole Awfulis
Lugeda tyybiperede kohta
Hargnevate andmetyypide eelised? Seosed tyybiperedega?
Klasside ja liikide vordsus - mis on seos Scala subtypinguga?
pattern matching in types
make names and types similar to Haskell
field name promotion
generalise branching types to branch not only over promoted algebraics but over Star, Int and Char
liigirakendamise eemaldamine liigituletuse kasuks (igal pool? teatud piiratud juhtudel?)
stringide syntaktiline suhkur
piiratud pesastatud mustrid hargnevas andmetüübis, nt: Construct_List (Mk_Pair T U) L ->
All Ord C, All c C <- klassimuutuja
polütüüpsus
check if things are put correctly into local or global context; (esp. classes and methods)
promote field names?
pattern match types with only one type constructor?
promote constructor operators (two type ops - for pair and either - and also promotable data constructor operators)
improve cat syntax
viia "kas asi on tõesti tüübikonstruktor?" kontroll tüüpijasse
in naming module: check name patterns and turn some into constructors without arguments
allow passing kind arguments to kind-polymorphic expressions: id{{Star}} etc.
reserve double colon for type specification; when single colon is promoted to type level it will cause a parsing conflict!
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Control.Monad
  import Data.Bifunctor
  import Data.Foldable
  import Data.List
  import Data.Map
  import Data.Maybe
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
  data Cat_4 = Cat_4 [String] [String] deriving Show
  data Cat_5 =
    Cat_5 Location_0 (String, [String]) [Name] ((Location_0, Pat'), (Location_0, Pat'), Data_br_2, Expression_1, Expression_1)
      deriving Show
  data Cat_6 = Cat_6 Location_0 (String, [String]) [Name] (TPat, TPat, Expression_1, Expression_1) deriving Show
  data Class_3 = Class_3 String [String] [String] (String, Kind_1) (Maybe (Name, [Kind_1])) [Method_3] deriving Show
  data Class_4 = Class_4 [String] [String] (String, Kind_1) (Maybe (String, [Kind_1])) [Method_4] deriving Show
  data Class_5 = Class_5 [String] [String] Kind_1 (Maybe (String, [Kind_1])) [String] deriving Show
  data Constraint_1 = Constraint_1 String [Kind_1] String deriving Show
  data Constructor = Constructor [String] [(TPat, Kind_1)] [Type_1] Type_1 [(String, Integer)] deriving Show
  data Data_3 = Data_3 String [(String, Kind_1)] Data_br_3 deriving Show
  data Data_br_3 =
    Algebraic_data_3 [Form_1] | Branching_data_3 String [Kind_1] [Data_case_3] | Struct_data_3 String [(String, Type_8)]
      deriving Show
  data Data_case_3 = Data_case_3 String [(String, Kind_1)] Data_br_3 deriving Show
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
  data Eqtn = Kind_eq Kind_1 Kind_1 | Type_eq Type_1 Type_1 deriving Show
  data Eqtns = Eqtns (Set String, Set String) [Eqtn] [Kind_1] [(String, (Name, Type_1))] deriving Show
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
      (Map' (Map' Inst))
      (Map' ([String], [String], Kind_1))
      (Map' Prom_alg)
      (Map' Cat_4)
      (Map' PConstructor)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Inst = Inst [Kind_1] [[String]] deriving Show
  data KT2 = KT2 [String] [String] [(String, Kind_1)] deriving Show
  data Kind = Arrow_kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_1)] [Constraint_1] Type_1 deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Show)
  data PConstructor = PConstructor [String] [Kind_1] Kind_1 [(String, Integer)] deriving Show
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
  data Polykind = Polykind (Maybe String) [String] Kind_1 deriving Show
  data Prom_alg = Prom_alg [String] (Map' [Kind_1]) deriving Show
  data TPat = Application_tpat String [TPat] | Name_tpat String deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String (Maybe Kind_1) [Kind_1] deriving (Eq, Show)
  data Type_2 = Type_2 (Maybe String) [String] [String] [(TPat, Kind_1)] (Maybe Constraint_1) [Constraint_1] Type_1
    deriving Show
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
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") a)
  arrow_type :: Kind_1 -> Type_1 -> Type_1 -> Type_1
  arrow_type k a = Application_type_1 (Application_type_1 (Name_type_1 "Arrow" (Just k) []) a)
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
  char_type :: Type_1
  char_type = ntype "Char"
  check_cat :: Location_1 -> Map' Cat_4 -> Kind_1 -> Err ()
  check_cat g a b =
    let
      (c, d) = kind_string b []
    in
      case Data.Map.lookup c a of
        Nothing -> Left ("Kind " ++ c ++ " should have an instance of Cat because of something going on" ++ location' g)
        Just (Cat_4 e f) -> check_cats' g a (zip d e) (Data.Set.fromList f)
  check_cats :: Location_1 -> Map' Cat_4 -> Map' Kind_1 -> [String] -> Err ()
  check_cats f e a b =
    case b of
      [] -> Right ()
      c : d -> check_cat f e (a ! c) *> check_cats f e a d
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
  check_kind :: String -> String -> Map' (Either Polykind Kind_1) -> Type_1 -> Err Kind_1
  check_kind j c a b =
    let
      x = Left j
    in
      case b of
        Application_type_1 d e ->
          do
            f <- check_kind j c a d
            case f of
              Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") g) h ->
                do
                  i <- check_kind j c a e
                  case i == g of
                    False -> x
                    True -> Right h
              _ -> x
        Name_type_1 d k e ->
          case d == c of
            False ->
              let
                (h, f, g) = check_kind' (a ! d)
              in
                Right (repkinds (Data.Map.fromList (zip (maybeToList h ++ f) (maybeToList k ++ e))) g)
            True -> x
  check_kind' :: Either Polykind Kind_1 -> (Maybe String, [String], Kind_1)
  check_kind' a =
    case a of
      Left (Polykind b c d) -> (b, c, d)
      Right b -> (Nothing, [], b)
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
              Method_4 "multiply" [] [] (function_type (ntype "T") (function_type (ntype "T") (ntype "T"))),
              Method_4 "negate" [] [] (function_type (ntype "T") (ntype "T"))]),
        (
          "Writeable",
          Class_4
            []
            []
            ("T", star_kind)
            Nothing
            [Method_4 "Write_Brackets" [] [] (function_type (ntype "T") (pair_type (list_type char_type) int_type))])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 e f (_, a) b c) -> Class_5 e f a b ((\(Method_4 d _ _ _) -> d) <$> c)) <$> classes_0
  classes_2 :: Map' ([String], [String], Kind_1)
  classes_2 = (\(Class_4 b c (_, a) _ _) -> (b, c, a)) <$> classes_0
  comparison_kind :: Kind_1
  comparison_kind = Name_kind_1 "Ordering"
  comparison_type :: Type_1
  comparison_type = ntype "Ordering"
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
  constructors :: Map' Constructor
  constructors =
    Data.Map.fromList
      [
        (
          "ConstructList",
          Constructor
            []
            [(Name_tpat "T", star_kind)]
            [ntype "T", list_type (ntype "T")]
            (list_type (ntype "T"))
            [("ConstructList", 2), ("EmptyList", 0)]),
        ("EQ", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        (
          "EmptyList",
          Constructor [] [(Name_tpat "T", star_kind)] [] (list_type (ntype "T")) [("ConstructList", 2), ("EmptyList", 0)]),
        ("GT", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        (
          "Just",
          Constructor [] [(Name_tpat "T", star_kind)] [ntype "T"] (maybe_type (ntype "T")) [("Just", 1), ("Nothing", 0)]),
        ("LT", Constructor [] [] [] comparison_type [("EQ", 0), ("GT", 0), ("LT", 0)]),
        (
          "Left",
          Constructor
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            [ntype "T"]
            (either_type (ntype "T") (ntype "U"))
            [("Left", 1), ("Right", 1)]),
        (
          "Mk_Pair",
          Constructor
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            [ntype "T", ntype "U"]
            (pair_type (ntype "T") (ntype "U"))
            [("Mk_Pair", 2)]),
        ("Next", Constructor [] [] [ntype "Nat"] (ntype "Nat") [("Next", 1), ("Zero", 0)]),
        ("Nothing", Constructor [] [(Name_tpat "T", star_kind)] [] (maybe_type (ntype "T")) [("Just", 1), ("Nothing", 0)]),
        (
          "Right",
          Constructor
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            [ntype "U"]
            (either_type (ntype "T") (ntype "U"))
            [("Left", 1), ("Right", 1)]),
        ("Zero", Constructor [] [] [] (ntype "Nat") [("Next", 1), ("Zero", 0)])]
  context_union :: (File, Map' Op) -> (File, Map' Op) -> (File, Map' Op)
  context_union (File i j d a x e q t g o z w', t0) (File k l h c y m r u n p p2 a', t2) =
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
        (Data.Map.union o p)
        (Data.Map.union z p2)
        (Data.Map.union w' a'),
      Data.Map.union t0 t2)
  defs :: Map' Expression_2
  defs =
    Data.Map.fromList
      [
        (
          "Compose Star",
          Function_expression_2
            (Name_pat_1 "f")
            (Function_expression_2
              (Name_pat_1 "g")
              (Function_expression_2
                (Name_pat_1 "x")
                (Application_expression_2
                  (Name_expression_2 "f")
                  (Application_expression_2 (Name_expression_2 "g") (Name_expression_2 "x")))))),
        (
          "ConstructList",
          Function_expression_2
            (Name_pat_1 "x")
            (Function_expression_2
              (Name_pat_1 "y")
              (Algebraic_expression_2 "ConstructList" [Name_expression_2 "x", Name_expression_2 "y"]))),
        ("EQ", Algebraic_expression_2 "EQ" []),
        ("EmptyList", Algebraic_expression_2 "EmptyList" []),
        ("First", Field_expression_2 0),
        ("GT", Algebraic_expression_2 "GT" []),
        ("Id Star", Function_expression_2 (Name_pat_1 "x") (Name_expression_2 "x")),
        ("Just", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Just" [Name_expression_2 "x"])),
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
        ("Next", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Next" [Name_expression_2 "x"])),
        ("Nothing", Algebraic_expression_2 "Nothing" []),
        ("Right", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Right" [Name_expression_2 "x"])),
        ("Second", Field_expression_2 1),
        ("Write_Brackets Int", Write_Brackets_Int_expression_2),
        ("Zero", Algebraic_expression_2 "Zero" []),
        ("add Int", Add_Int_0_expression_2),
        ("compare Char", Compare_Char_0_expression_2),
        ("compare Int", Compare_Int_0_expression_2),
        ("convert Int", Convert_Int_expression_2),
        ("div", Div_0_expression_2),
        ("multiply Int", Multiply_Int_0_expression_2),
        ("negate Int", Negate_Int_expression_2)]
  either_kind :: Kind_1 -> Kind_1 -> Kind_1
  either_kind x = Application_kind_1 (Application_kind_1 (Name_kind_1 "Either") x)
  either_type :: Type_1 -> Type_1 -> Type_1
  either_type x = Application_type_1 (Application_type_1 (ntype "Either") x)
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
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
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      ((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2) ->
      Alg_pat_1 ->
      Type_1 ->
      Err (((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2), (Alg_pat_2, Alg_pat_3)))
  get_pattern_type a b (d, e, f, n) g h =
    case g of
      Application_alg_pat_1 (Name o i) j ->
        und_err
          i
          b
          "constructor"
          (a o)
          (\(Constructor k u1 x m _) ->
            let
              (q, r, s) = typevars (k, u1) (d, Data.Map.empty, e)
            in
              (
                second (bimap (Application_alg_pat_2 i) (Struct_alg_pat_3 i)) <$>
                get_pattern_types a b (q, s, Type_eq h (repl' r m) : f, n) j (repl' r <$> x) (Name o i)))
      Blank_alg_pat_1 -> Right ((d, e, f, n), (Blank_alg_pat_2, Blank_alg_pat_3))
      Char_alg_pat_1 i -> Right ((d, e, Type_eq h char_type : f, n), (Char_alg_pat_2 i, Char_alg_pat_3 i))
      Int_alg_pat_1 i -> Right ((d, e, Type_eq h int_type : f, n), (Int_alg_pat_2 i, Int_alg_pat_3 i))
      Name_alg_pat_1 i ->
        Right ((d, e, f, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] h) n), (Name_alg_pat_2 i, Blank_alg_pat_3))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      ((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      Err (((Integer, Integer), (Set String, Set String), [Eqtn], Map' Type_2), ([Alg_pat_2], [Alg_pat_3])))
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
        ("Arrow", Arrow_kind (Arrow_kind Star_kind)),
        ("Either", Arrow_kind (Arrow_kind Star_kind)),
        ("List", Arrow_kind Star_kind),
        ("Maybe", Arrow_kind Star_kind),
        ("Nat", Star_kind),
        ("Ordering", Star_kind),
        ("Pair", Arrow_kind (Arrow_kind Star_kind)),
        ("Star", Star_kind)]
  init_type_context :: (File, Map' Op)
  init_type_context =
    (
      File
        kinds
        constructors
        types
        hkinds
        promotables
        classes_0
        classes_1
        instances
        classes_2
        prom_algs
        (Data.Map.singleton "Star" (Cat_4 [] []))
        pconstrs,
      Data.Map.empty)
  instances :: Map' (Map' Inst)
  instances =
    Data.Map.fromList
      [
        ("Ord", Data.Map.fromList [("Char", Inst [] []), ("Int", Inst [] [])]),
        ("Ring", Data.Map.fromList [("Int", Inst [] [])]),
        ("Writeable", Data.Map.fromList [("Int", Inst [] [])])]
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
        ("Arrow", Polykind (Just "K") [] (arrow_kind (Name_kind_1 "K") (arrow_kind (Name_kind_1 "K") (Name_kind_1 "Star")))),
        ("Char", pkind star_kind),
        (
          "ConstructList",
          Polykind
            Nothing
            ["K"]
            (arrow_kind (Name_kind_1 "K") (arrow_kind (list_kind (Name_kind_1 "K")) (list_kind (Name_kind_1 "K"))))),
        ("EQ", pkind comparison_kind),
        ("Either", pkind (arrow_kind star_kind (arrow_kind star_kind star_kind))),
        ("EmptyList", Polykind Nothing ["K"] (list_kind (Name_kind_1 "K"))),
        ("GT", pkind comparison_kind),
        ("Int", pkind star_kind),
        ("Just", Polykind Nothing ["K"] (arrow_kind (Name_kind_1 "K") (maybe_kind (Name_kind_1 "K")))),
        ("LT", pkind comparison_kind),
        ("Left", Polykind Nothing ["K", "L"] (arrow_kind (Name_kind_1 "K") (either_kind (Name_kind_1 "K") (Name_kind_1 "L")))),
        ("List", pkind (arrow_kind star_kind star_kind)),
        ("Maybe", pkind (arrow_kind star_kind star_kind)),
        (
          "Mk_Pair",
          Polykind
            Nothing
            ["K", "L"]
            (arrow_kind (Name_kind_1 "K") (arrow_kind (Name_kind_1 "L") (pair_kind (Name_kind_1 "K") (Name_kind_1 "L"))))),
        ("Nat", pkind star_kind),
        ("Next", pkind (arrow_kind nat_kind nat_kind)),
        ("Nothing", Polykind Nothing ["K"] (maybe_kind (Name_kind_1 "K"))),
        ("Ordering", pkind star_kind),
        ("Pair", pkind (arrow_kind star_kind (arrow_kind star_kind star_kind))),
        ("Right", Polykind Nothing ["K", "L"] (arrow_kind (Name_kind_1 "L") (either_kind (Name_kind_1 "K") (Name_kind_1 "L")))),
        ("Zero", pkind nat_kind)]
  kindvar :: String -> (Integer, Set String) -> (Integer, Set String)
  kindvar _ (b, c) = (b + 1, Data.Set.insert (show b) c)
  kindvars :: [String] -> (Integer, Set String) -> (Integer, Set String)
  kindvars a b =
    case a of
      [] -> b
      d : e -> kindvars e (kindvar d b)
  krep_eq :: (Kind_1 -> Kind_1) -> Eqtn -> Eqtn
  krep_eq a b =
    case b of
      Kind_eq c d -> Kind_eq (a c) (a d)
      Type_eq c d -> Type_eq (type_rep a c) (type_rep a d)
  kvars :: [String] -> (Integer, Map' Kind_1, Set String) -> ((Integer, Map' Kind_1, Set String), [String])
  kvars a (b, c, d) =
    let
      f = b + fromIntegral (length a)
      e = show <$> [b .. f - 1]
    in
      ((f, Data.Map.union c (Data.Map.fromList (zip a (Name_kind_1 <$> e))), Data.Set.union d (Data.Set.fromList e)), e)
  list_kind :: Kind_1 -> Kind_1
  list_kind = Application_kind_1 (Name_kind_1 "List")
  list_type :: Type_1 -> Type_1
  list_type = Application_type_1 (ntype "List")
  location_err' :: String -> Location_1 -> Location_1 -> String
  location_err' a b = location_err a (Library b)
  locations :: Locations
  locations =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        [
          "Arrow",
          "Char",
          "Compose",
          "ConstructList",
          "EQ",
          "Either",
          "EmptyList",
          "First",
          "GT",
          "Id",
          "Int",
          "Just",
          "LT",
          "Left",
          "List",
          "Maybe",
          "Mk_Pair",
          "Mod",
          "Nat",
          "Next",
          "Nothing",
          "Ord",
          "Ordering",
          "Pair",
          "Right",
          "Ring",
          "Second",
          "Star",
          "Write_Brackets",
          "Writeable",
          "Zero",
          "add",
          "compare",
          "convert",
          "div",
          "multiply",
          "negate",
          "undefined"])
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
  maybe_kind :: Kind_1 -> Kind_1
  maybe_kind = Application_kind_1 (Name_kind_1 "Maybe")
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (ntype "Maybe")
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "Nat"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  next_type :: Type_1 -> Type_1
  next_type = Application_type_1 (ntype "Next")
  ntype :: String -> Type_1
  ntype a = Name_type_1 a Nothing []
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
  pconstrs :: Map' PConstructor
  pconstrs =
    (
      (\(Constructor _ a b c d) -> PConstructor ((\(Name_tpat e, _) -> e) <$> a) (prom_type <$> b) (prom_type c) d) <$>
      constructors)
  pkind :: Kind_1 -> Polykind
  pkind = Polykind Nothing []
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
        ("Either", Prom_alg ["K", "L"] (Data.Map.fromList [("Left", [Name_kind_1 "K"]), ("Right", [Name_kind_1 "L"])])),
        (
          "List",
          Prom_alg
            ["K"]
            (Data.Map.fromList
              [
                ("ConstructList", [Name_kind_1 "K", Application_kind_1 (Name_kind_1 "List") (Name_kind_1 "K")]),
                ("EmptyList", [])])),
        ("Maybe", Prom_alg ["K"] (Data.Map.fromList [("Just", [Name_kind_1 "K"]), ("Nothing", [])])),
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
  promotables =
    Data.Map.fromList
      [
        ("Arrow", False),
        ("Char", False),
        ("Either", True),
        ("Int", False),
        ("List", True),
        ("Maybe", True),
        ("Nat", True),
        ("Ordering", True),
        ("Pair", True)]
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  rep_eq :: (Type_1 -> Type_1) -> Eqtn -> Eqtn
  rep_eq a b =
    case b of
      Kind_eq _ _ -> b
      Type_eq c d -> Type_eq (a c) (a d)
  repkinds :: Map' Kind_1 -> Kind_1 -> Kind_1
  repkinds a b =
    case b of
      Application_kind_1 c d -> Application_kind_1 (repkinds a c) (repkinds a d)
      Name_kind_1 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
  repkinds_method :: Map' Kind_1 -> Method_4 -> Method_4
  repkinds_method a (Method_4 b c d e) = Method_4 b (second (repkinds a) <$> c) d (repkinds_type a e)
  repkinds_type :: Map' Kind_1 -> Type_1 -> Type_1
  repkinds_type a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repkinds_type a c) (repkinds_type a d)
      Name_type_1 c e d -> Name_type_1 c (repkinds a <$> e) (repkinds a <$> d)
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c _ _ ->
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
  show_char :: Char -> String
  show_char c = show [c]
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
  solve_type_eqs :: Location_1 -> Set String -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> Err (Map' Kind_1, Type_1)
  solve_type_eqs j a b k =
    case b of
      [] -> Right k
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
    Location_1 -> Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> Err (Map' Kind_1, Type_1)
  solve_type_eqs' e a b c d k =
    case Data.Set.member b a of
      False -> kind_err e
      True ->
        case occ_kind b c of
          False -> type_reps e a b c d k
          True -> kind_err e
  solve_type_eqs_2 :: Set String -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> (Map' Kind_1, Type_1)
  solve_type_eqs_2 a b k =
    case b of
      [] -> k
      d : e ->
        case d of
          (Application_kind_1 f g, Application_kind_1 h i) -> solve_type_eqs_2 a ((f, h) : (g, i) : e) k
          (Application_kind_1 f g, Name_kind_1 h) -> type_reps_2 a h (Application_kind_1 f g) e k
          (Name_kind_1 f, Application_kind_1 g h) -> type_reps_2 a f (Application_kind_1 g h) e k
          (Name_kind_1 f, Name_kind_1 g) ->
            case f == g of
              False ->
                case Data.Set.member f a of
                  False -> type_reps_2 a g (Name_kind_1 f) e k
                  True -> type_reps_2 a f (Name_kind_1 g) e k
              True -> solve_type_eqs_2 a e k
  solvek' ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      Eqtn ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvek' m3 h b c d eq (y, x, m, (w, a)) =
    case Data.Set.member b w of
      False -> h
      True -> solvek_rep m3 h b c d (y, x, m, (w, a))
  solvek_rep ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Kind_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvek_rep m3 a c d e (y, x, f, (w, k)) =
    let
      m = kindrep c d
    in
      case occ_k c d of
        False -> solvesys m3 (krep_eq m <$> e) (m <$> y, second (second (type_rep m)) <$> x, f, (Data.Set.delete c w, k))
        True -> a
  solvesys ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys m b (a2, a', t, (w, u)) =
    case b of
      [] -> Right (a2, a', t, (w, u))
      eq : g ->
        case eq of
          Kind_eq c d ->
            let
              e3 = m "Kind" eq
            in
              case (c, d) of
                (Application_kind_1 e f, Application_kind_1 h i) ->
                  solvesys m (Kind_eq e h : Kind_eq f i : g) (a2, a', t, (w, u))
                (Name_kind_1 e, Name_kind_1 f) ->
                  case e == f of
                    False ->
                      case (Data.Set.member e w, Data.Set.member f w) of
                        (False, False) -> e3
                        (True, _) -> solvek_rep m e3 e d g (a2, a', t, (w, u))
                        (_, True) -> solvek_rep m e3 f c g (a2, a', t, (w, u))
                    True -> solvesys m g (a2, a', t, (w, u))
                (Name_kind_1 e, _) -> solvek' m e3 e d g eq (a2, a', t, (w, u))
                (_, Name_kind_1 e) -> solvek' m e3 e c g eq (a2, a', t, (w, u))
          Type_eq c d ->
            let
              e3 = m "Type" eq
            in
              case (c, d) of
                (Application_type_1 e f, Application_type_1 h i) ->
                  solvesys m (Type_eq e h : Type_eq f i : g) (a2, a', t, (w, u))
                (Name_type_1 e e0 e1, Name_type_1 f f0 f1) ->
                  case e == f of
                    False ->
                      case (Data.Set.member e u, Data.Set.member f u) of
                        (False, False) -> e3
                        (True, _) -> solvesys_rep m e3 e d g (a2, a', t, (w, u))
                        (_, True) -> solvesys_rep m e3 f c g (a2, a', t, (w, u))
                    True -> solvesys m (maybeToList (Kind_eq <$> e0 <*> f0) ++ zipWith Kind_eq e1 f1 ++ g) (a2, a', t, (w, u))
                (Name_type_1 e _ _, _) -> solvesys' m e3 e d g (a2, a', t, (w, u))
                (_, Name_type_1 e _ _) -> solvesys' m e3 e c g (a2, a', t, (w, u))
  solvesys' ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys' m3 h b c d (y, x, m, (w, a)) =
    case Data.Set.member b a of
      False -> h
      True -> solvesys_rep m3 h b c d (y, x, m, (w, a))
  solvesys_rep ::
    (
      (String -> Eqtn -> Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      String ->
      Type_1 ->
      [Eqtn] ->
      ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([Kind_1], [(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys_rep m3 a c d e (y, x, f, (w, k)) =
    let
      m = sysrep' c d
    in
      case occ_check c d of
        False -> solvesys m3 (rep_eq m <$> e) (y, second (second m) <$> x, sysrep2 c d f, (w, Data.Set.delete c k))
        True -> a
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
        (Locations, Locations, Locations, Map' (Map' Location')),
        (File, Map' Op),
        Map' Expression_2,
        Map' ([String], Map' [(String, Nat)])) ->
      Err
        (
          (Locations, Locations, Locations, Map' (Map' Location')),
          (File, Map' Op),
          Map' Expression_2,
          Map' ([String], Map' [(String, Nat)])))
  standard_naming_typing f a (b, (c, t), g, w) =
    (
      standard_1 (Location_1 f) t a >>=
      \(v, n') -> naming f n' b >>= \(d, e) -> (\(h, i, n) -> (d, (h, v), i, n)) <$> typing f e (c, g, w))
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
        Name_type_1 d _ _ -> if d == a then b else c
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
  type_case ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      (Alg_pat_1, Expression_1) ->
      Integer ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ((Alg_pat_2, Typedexpr), Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a c d (Eqtns e p y' q) f (g, h) i j k =
    (
      get_pattern_type c a (d, e, p, f) g (ntype (show i)) >>=
      \((m, n, s, t), (o, y)) -> (\(u, v, w, r0) -> ((o, u), v, w, r0, y)) <$> type_expression a c m (Eqtns n s y' q) t h j k)
  type_cases ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      [(Alg_pat_1, Expression_1)] ->
      Integer ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ([(Alg_pat_2, Typedexpr)], Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])], [Alg_pat_3]))
  type_cases b c d e f g n h i =
    case g of
      [] -> Right ([], e, d, [], [])
      l : m ->
        (
          type_case b c d e f l n h i >>=
          \(o, p, q, r0, r') -> (\(r, s, t, r1, w) -> (o : r, s, t, r0 ++ r1, r' : w)) <$> type_cases b c q p f m n h i)
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
              \(_, s) -> Right (ins_new p (Cat_4 d s) j, Cat_5 b (p, d) n e))
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
                      type_data_br_2 (Location_1 a) (arrow_type x e4 e5) k8 t3 y (k, l) (d, [(v, x), (a1, x)]) "Arrow" f7 j >>=
                      \(k7, f0) -> Right ((k7, f0, d3), Cat_6 b (p, d) n (v, a1, h, i))))))
  type_cat_2 ::
    (
      (
        Location_0 -> Location_1,
        Map' Kind,
        Map' PConstructor,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' Constructor,
        Map' Type_2) ->
      Cat_6 ->
      (Map' Expression_2) ->
      Err (Map' Expression_2))
  type_cat_2 (a, o, t, u, m', z', j, k7, l) (Cat_6 b (p, d) n (e, f, h, i)) m =
    let
      x = Prelude.foldl Application_kind_1 (Name_kind_1 p) (Name_kind_1 <$> d)
      y = Data.Map.union o (Data.Map.fromList ((\y' -> (y', Star_kind)) <$> d))
      s' m7 fj u1 n2 =
        type_expr (m7 ++ " " ++ p ++ location' (a b)) fj a (k7, l) u1 z' n2 m' (type_tpat_2 t f x (type_tpat_2 t e x u), y) j b
    in
      (
        (\j7 -> \j8 -> Data.Map.insert ("Id " ++ p) j7 (Data.Map.insert ("Compose " ++ p) j8 m)) <$>
        s'
          "Compose"
          (function_type
            (arrow_type x (ntype "0") (ntype "1"))
            (function_type (arrow_type x (ntype "2") (ntype "0")) (arrow_type x (ntype "2") (ntype "1"))))
          h
          3 <*>
        s' "Id" (arrow_type x (ntype "0") (ntype "0")) i 1)
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
  type_cats_2 ::
    (
      (
        Location_0 -> Location_1,
        Map' Kind,
        Map' PConstructor,
        Map' Polykind,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' Inst),
        Map' Cat_4,
        Map' Constructor,
        Map' Type_2) ->
      [Cat_6] ->
      (Map' Expression_2) ->
      Err (Map' Expression_2))
  type_cats_2 a b c =
    case b of
      [] -> Right c
      d : e -> type_cat_2 a d c >>= type_cats_2 a e
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
                type_inh b [b] ((\(Name _ t4, _) -> t4) <$> g') x2 *>
                (
                  (case g' of
                    Nothing -> Right Nothing
                    Just (t4, m2) -> (\x3 -> Just (t4, x3)) <$> traverse (type_kind_7 a i Star_kind) m2) >>=
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
                                Just (Name _ t0, _) -> Data.Map.insert b t0 x2
                                Nothing -> x2))) <$>
                      type_methods_0 a e (Data.Map.insert c (pkind h) j) i2 e7)))))
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
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") l) f ->
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
  type_constraints_1 :: [Constraint_1] -> Map' (Map' Inst) -> Map' Class_4 -> Map' (Map' Inst)
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
  type_def_2 ::
    (
      (Location_0 -> Location_1) ->
      Def_4 ->
      (Map' Constructor, Map' Type_2) ->
      Map' Expression_2 ->
      Map' (Map' Inst) ->
      Map' Polykind ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      Map' Kind ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_def_2 j a d c m n t' u0 v2 f1 =
    case a of
      Basic_def_4 r e (KT2 b0 g3 b) x h i y' ->
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
              f1
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
                      Just (Inst u4 u) ->
                        (
                          check_cats_2 (j l') f1 u4 *>
                          case constr_check u0 (fst <$> e0) u r' of
                            Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                            Nothing -> r)
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
  type_defs_1 ::
    (
      String ->
      Map' Kind ->
      [Def_3] ->
      Map' Polykind ->
      Types ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err ([Def_4], Types, (Map' (Map' (Inst, Status)), Map' ([String], Map' [(String, Nat)]))))
  type_defs_1 h x a b c y y0 v u =
    case a of
      [] -> Right ([], c, u)
      d : e ->
        type_def_1 h x d b c y y0 v u >>= \(f, g, u') -> (\(k, l, m) -> (f : k, l, m)) <$> type_defs_1 h x e b g y y0 v u'
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' Constructor, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' Inst) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Kind ->
    Map' Cat_4 ->
    Err (Map' Expression_2)
  type_defs_2 f a b c g i j u v h' =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i j u v h' >>= \h -> type_defs_2 f e b h g i j u v h'
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
  type_expr ::
    (
      String ->
      Type_1 ->
      (Location_0 -> Location_1) ->
      (Map' Constructor, Map' Type_2) ->
      Expression_1 ->
      Map' (Map' Inst) ->
      Integer ->
      Map' ([String], Map' [(String, Nat)]) ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Location_0 ->
      Err Expression_2)
  type_expr k h a (c, e) f m w w' b a3 l3 =
    let
      n = " in " ++ k
    in
      (
        type_expression c a (0, w) (Eqtns (Data.Set.empty, Data.Set.empty) [] [] []) e f h b >>=
        \(g, (Eqtns i j q8 x), _, x3) ->
          (
            solvesys
              (\w2 -> \eq ->
                let
                  (y, p) =
                    case eq of
                      Kind_eq k7 k8 -> (write_kind k7, write_kind k8)
                      Type_eq t7 t8 -> (write_type t7, write_type t8)
                in
                  Left (w2 ++ " mismatch between " ++ y ++ " and " ++ p ++ n))
              j
              (q8, x, g, i) >>=
            \(q9, y, p, (k2, k')) ->
              case Data.Set.null k2 of
                False -> Left ("Unresolved kind variables" ++ n)
                True ->
                  case Data.Set.null k' of
                    False -> Left ("Unresolved type variables" ++ n)
                    True ->
                      (
                        addargs w' p <$
                        traverse_ (check_cat (a l3) a3) q9 <*
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
                        pats a ((\(Constructor _ _ _ _ e') -> e') <$> c) x3)))
  type_expr' ::
    (
      (Map' Polykind, Map' Constructor, Map' Type_2, Map' Kind) ->
      Expression_1 ->
      Map' (Map' Inst) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Cat_4 ->
      Err Expression_2)
  type_expr' (b, c, e, i) f g h c3 =
    type_expr
      "input."
      (list_type char_type)
      (Location_1 "input")
      (c, e)
      (Application_expression_1
        (Name_expression_1 (Name (Location_0 0 0) "First"))
        (Application_expression_1 (Name_expression_1 (Name (Location_0 0 0) "Write_Brackets")) f))
      g
      0
      h
      (b, i)
      c3
      (Location_0 0 0)
  type_expression ::
    (
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      Expression_1 ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err (Typedexpr, Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])]))
  type_expression v r (o', o) (Eqtns (f4, f) h mi c') d b e (r7, m8) =
    case b of
      Application_expression_1 c g ->
        (
          type_expression
            v
            r
            (o', o + 1)
            (Eqtns (f4, Data.Set.insert (show o) f) h mi c')
            d
            c
            (function_type (ntype (show o)) e)
            (r7, m8) >>=
          \(i, j, p, d7) ->
            (
              (\(l, m, q, e') -> (Application_texpr i l, m, q, d7 ++ e')) <$>
              type_expression v r p j d g (ntype (show o)) (r7, m8)))
      Char_expression_1 c -> Right (Char_texpr c, Eqtns (f4, f) (Type_eq e char_type : h) mi c', (o', o), [])
      Function_expression_1 c g ->
        (
          type_pat r v c (ntype (show o)) d (o', o + 1) (f4, Data.Set.insert (show o) f) h >>=
          \(a6, b6, (c2, c6), (f5, d6), f6) ->
            (
              (\(a', b', d', f7) -> (Function_texpr a6 a', b', d', f7)) <$>
              type_expression
                v
                r
                (c2, c6 + 1)
                (Eqtns
                  (f5, Data.Set.insert (show c6) d6)
                  (Type_eq e (function_type (ntype (show o)) (ntype (show c6))) : f6)
                  mi
                  c')
                b6
                g
                (ntype (show c6))
                (r7, m8)))
      Int_expression_1 c -> Right (Int_texpr c, Eqtns (f4, f) (Type_eq e int_type : h) mi c', (o', o), [])
      Match_expression_1 a7 c g ->
        (
          type_expression v r (o', o + 1) (Eqtns (f4, Data.Set.insert (show o) f) h mi c') d c (ntype (show o)) (r7, m8) >>=
          \(k, m, n, n2) ->
            (\(q, u, x, n3, n4) -> (Match_texpr k q, u, x, n2 ++ [(a7, n4)] ++ n3)) <$> type_cases v r n m d g o e (r7, m8))
      Name_expression_1 (Name a7 c) ->
        und_err
          c
          d
          "variable"
          (r a7)
          (\(Type_2 m2 i3 m3 i x0 a' j) ->
            let
              a8 = maybeToList m2 ++ i3
              o9 = o' + fromIntegral (length a8)
              f6 = show <$> [o' .. o9 - 1]
              d1 = Data.Map.fromList (zip a8 (Name_kind_1 <$> f6))
              (f0, p, kl) = typevars_0 (second (repkinds d1) <$> i) (o, Data.Map.empty, f)
              x7 = (\(Constraint_1 a0 _ b0) -> (a0, (Name a7 c, p ! b0))) <$> a'
            in
              Right
                (
                  case x0 of
                    Nothing -> Name_texpr_1 c (second snd <$> x7)
                    Just (Constraint_1 y0 _ _) -> Name_texpr_0 c y0 (Name_type_1 (show o) Nothing []),
                  Eqtns
                    (Data.Set.union f4 (Data.Set.fromList f6), kl)
                    ([Type_eq e (repl' p (repkinds_type d1 j))] ++ h)
                    ((Name_kind_1 <$> m3) ++ mi)
                    (x7 ++ c'),
                  (o9, f0),
                  []))
  type_exprs ::
    (
      (Name -> String) ->
      (Location_0 -> Location_1) ->
      (Map' Constructor, Map' Type_2) ->
      Map' (Map' Inst) ->
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)] ->
      (Map' Expression_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      (Expression_2 -> Expression_2) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      (Map' Polykind, Map' Kind) ->
      Map' Cat_4 ->
      Err (Map' Expression_2))
  type_exprs a b c d h i t z w f' t' t0 (x2, t4) t9 =
    case h of
      [] -> Right i
      (j @ (Name l2 y), k, s, t5, l) : m ->
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
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' (pkind m0) k') x2 s, t4)
            t9
            l2 >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (f' g) i) t z w f' t' t0 (x2, t4) t9)
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
  type_pat ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      Pat' ->
      Type_1 ->
      Map' Type_2 ->
      (Integer, Integer) ->
      (Set String, Set String) ->
      [Eqtn] ->
      Err (Pat_1, Map' Type_2, (Integer, Integer), (Set String, Set String), [Eqtn]))
  type_pat a b c d e f g h =
    case c of
      Application_pat' (Name i j) k ->
        und_err
          j
          b
          "constructor"
          (a i)
          (\(Constructor l m n o p) ->
            case p of
              [_] ->
                let
                  (q, r, s) = typevars (l, m) (f, Data.Map.empty, g)
                in
                  (
                    (\(t, u, v, w, x) -> (Application_pat_1 t, u, v, w, x)) <$>
                    type_pats a b k (repl' r <$> n) e q s (Type_eq d (repl' r o) : h) (Name i j))
              _ -> Left ("Constructor " ++ j ++ location (a i) ++ " is not a struct constructor."))
      Blank_pat' -> Right (Blank_pat_1, e, f, g, h)
      Name_pat' i -> Right (Name_pat_1 i, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] d) e, f, g, h)
  type_pats ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      [Pat'] ->
      [Type_1] ->
      Map' Type_2 ->
      (Integer, Integer) ->
      (Set String, Set String) ->
      [Eqtn] ->
      Name ->
      Err ([Pat_1], Map' Type_2, (Integer, Integer), (Set String, Set String), [Eqtn]))
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
                a0,
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
    Location_1 -> Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> Err (Map' Kind_1, Type_1)
  type_reps e a b c d (k, l) =
    let
      f = kindrep b c
    in
      solve_type_eqs e (Data.Set.delete b a) (bimap f f <$> d) (f <$> k, type_rep f l)
  type_reps_2 :: Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> (Map' Kind_1, Type_1)
  type_reps_2 a b c d (k, l) =
    let
      f = kindrep b c
    in
      solve_type_eqs_2 (Data.Set.delete b a) (bimap f f <$> d) (f <$> k, type_rep f l)
  type_tpat ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      Pat' ->
      Kind_1 ->
      Map' Kind_1 ->
      Integer ->
      Set String ->
      [(Kind_1, Kind_1)] ->
      Integer ->
      Err ((TPat, Type_1), Map' Kind_1, Integer, Set String, [(Kind_1, Kind_1)], Integer))
  type_tpat k h b c d l n o d2 =
    case b of
      Application_pat' (Name g e) f ->
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
      Blank_pat' ->
        let
          a8 = "blank " ++ show d2
        in
          Right ((Name_tpat a8, ntype a8), Data.Map.insert a8 c d, l, n, o, d2 + 1)
      Name_pat' e -> Right ((Name_tpat e, ntype e), Data.Map.insert e c d, l, n, o, d2)
  type_tpat' ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      (Location_0, Pat') ->
      Kind_1 ->
      Map' Polykind ->
      Err ((TPat, Type_1), Map' Polykind, Map' Kind_1))
  type_tpat' a b (l, c) d e =
    (
      type_tpat a b c d Data.Map.empty 0 Data.Set.empty [] 0 >>=
      \((f, u2), g, _, i, j, _) ->
        (\(h, m1) -> ((f, m1), Data.Map.union e (pkind <$> h), h)) <$> solve_type_eqs (a l) i j (g, u2))
  type_tpat_2 :: Map' PConstructor -> TPat -> Kind_1 -> Map' Polykind -> Map' Polykind
  type_tpat_2 b c d e =
    let
      (u2, g, _, i, j) = type_tpat_3 b c d Data.Map.empty 0 Data.Set.empty []
    in
      Data.Map.union e (pkind <$> (fst (solve_type_eqs_2 i j (g, u2))))
  type_tpat_3 ::
    (
      Map' PConstructor ->
      TPat ->
      Kind_1 ->
      Map' Kind_1 ->
      Integer ->
      Set String ->
      [(Kind_1, Kind_1)] ->
      (Type_1, Map' Kind_1, Integer, Set String, [(Kind_1, Kind_1)]))
  type_tpat_3 h b c d l n o =
    case b of
      Application_tpat e f ->
        let
          PConstructor i j m _ = h ! e
          ((p, q, r), a2) = kvars i (l, Data.Map.empty, n)
          (s, t, u, v, w) = type_tpats_2 h f (repkinds q <$> j) d p r ((c, repkinds q m) : o) e
        in
          (Prelude.foldl Application_type_1 (Name_type_1 e Nothing (Name_kind_1 <$> a2)) s, t, u, v, w)
      Name_tpat e -> (ntype e, Data.Map.insert e c d, l, n, o)
  type_tpats ::
    (
      (Location_0 -> Location_1) ->
      Map' PConstructor ->
      [Pat'] ->
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
  type_tpats_2 ::
    (
      Map' PConstructor ->
      [TPat] ->
      [Kind_1] ->
      Map' Kind_1 ->
      Integer ->
      Set String ->
      [(Kind_1, Kind_1)] ->
      String ->
      ([Type_1], Map' Kind_1, Integer, Set String, [(Kind_1, Kind_1)]))
  type_tpats_2 b d e f g h i y =
    case (d, e) of
      (j : k, m : n) ->
        let
          (c, o, p, q, r) = type_tpat_3 b j m f g h i
          (s, t, u, v, w) = type_tpats_2 b k n o p q r y
        in
          (c : s, t, u, v, w)
      _ -> ([], f, g, h, i)
  type_typ :: (Location_0 -> Location_1) -> Map' Polykind -> Map' Kind -> Kind_1 -> Type_8 -> Map' Cat_4 -> Err Type_1
  type_typ a d e f (Type_8 b c) j =
    (
      type_eqs a 0 c d e f (Data.Set.empty, [], []) >>=
      \(_, (g, h, t), i) -> snd <$> solve_type_eqs (a b) g h (Data.Map.empty, i) <* check_cats_2 (a b) j t)
  type_types :: (Location_0 -> Location_1) -> [Type_8] -> Map' Polykind -> Map' Kind -> Map' Cat_4 -> Err [Type_1]
  type_types f a b g h =
    case a of
      [] -> Right []
      c : d -> type_typ f b g star_kind c h >>= \e -> (:) e <$> type_types f d b g h
  types :: Map' Type_2
  types =
    Data.Map.fromList
      [
        (
          "Compose",
          Type_2
            (Just "K")
            []
            []
            [(Name_tpat "T", Name_kind_1 "K"), (Name_tpat "U", Name_kind_1 "K"), (Name_tpat "V", Name_kind_1 "K")]
            Nothing
            []
            (function_type
              (arrow_type (Name_kind_1 "K") (ntype "T") (ntype "U"))
              (function_type
                (arrow_type (Name_kind_1 "K") (ntype "V") (ntype "T"))
                (arrow_type (Name_kind_1 "K") (ntype "V") (ntype "U"))))),
        (
          "ConstructList",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (function_type (list_type (ntype "T")) (list_type (ntype "T"))))),
        ("EQ", Type_2 Nothing [] [] [] Nothing [] comparison_type),
        ("EmptyList", Type_2 Nothing [] [] [(Name_tpat "T", star_kind)] Nothing [] (list_type (ntype "T"))),
        (
          "First",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            Nothing
            []
            (function_type (pair_type (ntype "T") (ntype "U")) (ntype "T"))),
        ("GT", Type_2 Nothing [] [] [] Nothing [] comparison_type),
        (
          "Id",
          Type_2
            (Just "K")
            []
            []
            [(Name_tpat "T", Name_kind_1 "K")]
            Nothing
            []
            (arrow_type (Name_kind_1 "K") (ntype "T") (ntype "T"))),
        (
          "Just",
          Type_2 Nothing [] [] [(Name_tpat "T", star_kind)] Nothing [] (function_type (ntype "T") (maybe_type (ntype "T")))),
        (
          "Left",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (either_type (ntype "T") (ntype "U")))),
        ("LT", Type_2 Nothing [] [] [] Nothing [] comparison_type),
        (
          "Mk_Pair",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            Nothing
            []
            (function_type (ntype "T") (function_type (ntype "U") (pair_type (ntype "T") (ntype "U"))))),
        ("Mod", Type_2 Nothing [] [] [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        ("Next", Type_2 Nothing [] [] [] Nothing [] (function_type nat_type nat_type)),
        ("Nothing", Type_2 Nothing [] [] [(Name_tpat "T", star_kind)] Nothing [] (maybe_type (ntype "T"))),
        (
          "Right",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            Nothing
            []
            (function_type (ntype "U") (either_type (ntype "T") (ntype "U")))),
        (
          "Second",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind), (Name_tpat "U", star_kind)]
            Nothing
            []
            (function_type (pair_type (ntype "T") (ntype "U")) (ntype "U"))),
        (
          "Write_Brackets",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Writeable" [] "T"))
            [Constraint_1 "Writeable" [] "T"]
            (function_type (ntype "T") (pair_type (list_type char_type) int_type))),
        ("Zero", Type_2 Nothing [] [] [] Nothing [] nat_type),
        (
          "add",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ring" [] "T"))
            [Constraint_1 "Ring" [] "T"]
            (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))),
        (
          "compare",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ord" [] "T"))
            [Constraint_1 "Ord" [] "T"]
            (function_type (ntype "T") (function_type (ntype "T") comparison_type))),
        (
          "convert",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ring" [] "T"))
            [Constraint_1 "Ring" [] "T"]
            (function_type int_type (ntype "T"))),
        ("div", Type_2 Nothing [] [] [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        (
          "multiply",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ring" [] "T"))
            [Constraint_1 "Ring" [] "T"]
            (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))),
        (
          "negate",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ring" [] "T"))
            [Constraint_1 "Ring" [] "T"]
            (function_type (ntype "T") (ntype "T"))),
        ("undefined", Type_2 Nothing [] [] [(Name_tpat "T", star_kind)] Nothing [] (ntype "T"))]
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b _ _ -> (b, d)
  typevar :: TPat -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevar a (b, c, d) =
    case a of
      Application_tpat _ e -> typevar' e (b, c, d)
      Name_tpat e ->
        let
          f = show b
        in
          (1 + b, Data.Map.insert e (ntype f) c, Data.Set.insert f d)
  typevar' :: [TPat] -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevar' a b =
    case a of
      [] -> b
      c : d -> typevar' d (typevar c b)
  typevars ::
    (
      ([String], [(TPat, Kind_1)]) ->
      ((Integer, Integer), Map' Type_1, (Set String, Set String)) ->
      ((Integer, Integer), Map' Type_1, (Set String, Set String)))
  typevars (a, b) ((c, d), e, (f, g)) =
    let
      (h, i) = kindvars a (c, f)
      (j, k, l) = typevars_0 b (d, e, g)
    in
      ((h, j), k, (i, l))
  typevars_0 :: [(TPat, Kind_1)] -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevars_0 a b =
    case a of
      [] -> b
      (c, _) : d -> typevars_0 d (typevar c b)
  typing ::
    String ->
    Tree_5 ->
    (File, Map' Expression_2, Map' ([String], Map' [(String, Nat)])) ->
    Err (File, Map' Expression_2, Map' ([String], Map' [(String, Nat)]))
  typing a (Tree_5 b c d f) (File g h i j k l m n o p q r, s, t) =
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
                      \((m', n', o'), p') ->
                        (
                          type_classes a (fst <$> u) (fst <$> f') d (fst <$> i') (old l, n', t, old o, old m) >>=
                          \(q', r', s', t', u') ->
                            (
                              type_defs_1
                                a
                                (fst <$> u)
                                f
                                (fst <$> f')
                                r'
                                (fst <$> q')
                                (fst <$> u')
                                (fst <$> i')
                                (old' n, s') >>=
                              \(v', w', (x', y')) ->
                                (
                                  type_cats_2
                                    (
                                      Location_1 a,
                                      fst <$> u,
                                      fst <$> e',
                                      fst <$> f',
                                      y',
                                      fmap fst <$> x',
                                      fst <$> i',
                                      fst <$> m',
                                      fst <$> w')
                                    p'
                                    o' >>=
                                  \a0 ->
                                    (
                                      (\b0 ->
                                        (
                                          File
                                            (rem_old f')
                                            (rem_old m')
                                            (rem_old w')
                                            (rem_old u)
                                            (unsafe_left <$> rem_old z)
                                            (rem_old q')
                                            (rem_old u')
                                            (rem_old' x')
                                            (rem_old t')
                                            (rem_old c')
                                            (rem_old i')
                                            (rem_old e'),
                                          b0,
                                          y')) <$>
                                      type_defs_2
                                        (Location_1 a)
                                        v'
                                        (fst <$> m', fst <$> w')
                                        a0
                                        ((<$>) fst <$> x')
                                        (fst <$> f')
                                        y'
                                        (fst <$> q')
                                        (fst <$> u)
                                        (fst <$> i'))))))))))
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
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
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
              Just e -> "{{" ++ write_kind e ++ "}}") ++
            case d of
              [] -> ""
              _ -> "[[" ++ intercalate ", " (write_kind <$> d) ++ "]]"),
          False)
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