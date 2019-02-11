{-
todo: make a function writing operator/function. For printing stuff like "Complex (Fraction 0 1) (Fraction 1 1)"
Let f = Crash, x = f f In 0 -- tüüpimine läheb lõpmatusse tsüklisse sest puudub occur check
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
  data Class_3 = Class_3 String [String] [String] (String, Kind_1) (Maybe (Name, [Kind_1])) [Method_3] deriving Show
  data Class_4 = Class_4 [String] [String] (String, Kind_1) (Maybe (String, [Kind_1])) [Method_4] deriving Show
  data Class_5 = Class_5 [String] [String] Kind_1 (Maybe (String, [Kind_1])) [String] deriving Show
  data Constraint_1 = Constraint_1 String [Kind_1] String deriving Show
  data Constructor = Constructor [String] [(TPat, Kind_1)] [Type_1] Type_1 [(String, Integer)] deriving Show
  data Data_3 = Data_3 String [String] [(String, Kind_1)] Data_br_3 deriving Show
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
  data Eqtns = Eqtns (Set String, Set String) [Kind_1] [(Type_1, Type_1)] [(String, (Name, Type_1))] deriving Show
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
      (Map' ([String], [String], Kind_1))
      (Map' Prom_alg)
      (Map' Cat_4)
      (Map' PConstructor)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data KT2 = KT2 [String] [String] [(String, Kind_1)] deriving Show
  data Kind = Arrow_kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_1)] [Constraint_1] Type_1 deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
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
  data Type_1 =
    Application_type_1 Type_1 Type_1 | Char_type_1 Char | Int_type_1 Integer | Name_type_1 String (Maybe Kind_1) [Kind_1]
      deriving (Eq, Show)
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
  char_kind :: Kind_1
  char_kind = Name_kind_1 "Char"
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
  check_kind :: String -> String -> Map' (Either Polykind Kind_1) -> Type_1 -> Err Kind_1
  check_kind j c a b =
    let
      x = Left j
    in
      case b of
        Application_type_1 d e -> check_kind j c a d >>= \f -> case f of
          Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") g) h ->
            check_kind j c a e >>= \i -> if i == g then Right h else x
          _ -> x
        Char_type_1 _ -> Right char_kind
        Int_type_1 _ -> Right int_kind
        Name_type_1 d k e ->
          if d == c
            then x
            else
              let
                (h, f, g) = check_kind' (a ! d)
              in
                Right (repkinds (Data.Map.fromList (zip (maybeToList h ++ f) (maybeToList k ++ e))) g)
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
      Name_type_5 (Name a d) m e ->
        case (m, e) of
          (Nothing, []) -> Just (if Data.Set.member d f then c else Data.Map.insert d a c)
          _ -> Nothing
      _ -> Nothing
  gather_types :: Set String -> [Type_8] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_types a b = gather_all_types (gather_type a) ((\(Type_8 _ c) -> c) <$> b)
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      ((Integer, Integer), (Set String, Set String), [(Type_1, Type_1)], Map' Type_2) ->
      Alg_pat_1 ->
      Type_1 ->
      Err (((Integer, Integer), (Set String, Set String), [(Type_1, Type_1)], Map' Type_2), (Alg_pat_2, Alg_pat_3)))
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
                get_pattern_types a b (q, s, (h, repl' r m) : f, n) j (repl' r <$> x) (Name o i)))
      Blank_alg_pat_1 -> Right ((d, e, f, n), (Blank_alg_pat_2, Blank_alg_pat_3))
      Char_alg_pat_1 i -> Right ((d, e, (h, char_type) : f, n), (Char_alg_pat_2 i, Char_alg_pat_3 i))
      Int_alg_pat_1 i -> Right ((d, e, (h, int_type) : f, n), (Int_alg_pat_2 i, Int_alg_pat_3 i))
      Name_alg_pat_1 i ->
        Right ((d, e, f, Data.Map.insert i (Type_2 Nothing [] [] [] Nothing [] h) n), (Name_alg_pat_2 i, Blank_alg_pat_3))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      ((Integer, Integer), (Set String, Set String), [(Type_1, Type_1)], Map' Type_2) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      Err (((Integer, Integer), (Set String, Set String), [(Type_1, Type_1)], Map' Type_2), ([Alg_pat_2], [Alg_pat_3])))
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
        ("Char", Star_kind),
        ("Either", Arrow_kind (Arrow_kind Star_kind)),
        ("Int", Star_kind),
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
  make_eq (Data_2 a d b c) =
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
  maybe_type = Application_type_1 (ntype "Maybe")
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "Nat"
  nat_type :: Type_1
  nat_type = ntype "Nat"
  next_type :: Type_1 -> Type_1
  next_type = Application_type_1 (ntype "Next")
  ntype :: String -> Type_1
  ntype a = Name_type_1 a Nothing []
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
  promotable' a =
    case a of
      Name_kind_0 (Name _ "Star") -> True
      _ -> False
  promotables :: Map' Bool
  promotables =
    Data.Map.fromList ((\a -> (a, True)) <$> ["Char", "Either", "Int", "List", "Maybe", "Nat", "Ordering", "Pair"])
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
  repkinds_method :: Map' Kind_1 -> Method_4 -> Method_4
  repkinds_method a (Method_4 b c d e) = Method_4 b (second (repkinds a) <$> c) d (repkinds_type a e)
  repkinds_type :: Map' Kind_1 -> Type_1 -> Type_1
  repkinds_type a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repkinds_type a c) (repkinds_type a d)
      Name_type_1 c e d -> Name_type_1 c (repkinds a <$> e) (repkinds a <$> d)
      _ -> b
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c _ _ ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
      _ -> b
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
  solvesys ::
    (
      (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      [(Type_1, Type_1)] ->
      ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys m b (a', t, (w, u)) =
    case b of
      [] -> Right (a', t, (w, u))
      (c, d) : g ->
        case c of
          Application_type_1 e f ->
            case d of
              Application_type_1 h i -> solvesys m ((e, h) : (f, i) : g) (a', t, (w, u))
              Name_type_1 h _ _ -> solvesys' m h c g (a', t, (w, u))
              _ -> undefined
          Char_type_1 e ->
            case d of
              Char_type_1 f -> if e == f then solvesys m g (a', t, (w, u)) else m (show e) (show f)
              Name_type_1 f _ _ -> solvesys' m f c g (a', t, (w, u))
              _ -> undefined
          Int_type_1 e ->
            case d of
              Int_type_1 f -> if e == f then solvesys m g (a', t, (w, u)) else m ('!' : show e) ('!' : show f)
              Name_type_1 f _ _ -> solvesys' m f c g (a', t, (w, u))
              _ -> undefined
          Name_type_1 e _ _ ->
            case d of
              Name_type_1 f _ _ ->
                if e == f
                  then solvesys m g (a', t, (w, u))
                  else
                    case Data.Set.member e u of
                      False ->
                        case Data.Set.member f u of
                          False -> m e f
                          True -> solvesys_rep m f c g (a', t, (w, u))
                      True -> solvesys_rep m e d g (a', t, (w, u))
              _ -> solvesys' m e d g (a', t, (w, u))
  solvesys' ::
    (
      (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Type_1 ->
      [(Type_1, Type_1)] ->
      ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys' h b c d (x, m, (w, a)) =
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
        True -> solvesys_rep h b c d (x, m, (w, a))
  solvesys_rep ::
    (
      (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String))) ->
      String ->
      Type_1 ->
      [(Type_1, Type_1)] ->
      ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)) ->
      Err ([(String, (Name, Type_1))], Typedexpr, (Set String, Set String)))
  solvesys_rep a c d e (x, f, (w, k)) =
    let
      m = sysrep' c d
    in
      solvesys a ((<$>) (bimap m m) e) (second (second m) <$> x, sysrep2 c d f, (w, Data.Set.delete c k))
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
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      [Kind_1] ->
      Err (Map' (Constructor, Status), Types))
  type_branching_1 a b c (Data_case_3 d e f) g h i (k', k) l m n =
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
      (Integer, Integer) ->
      Eqtns ->
      Map' Type_2 ->
      (Alg_pat_1, Expression_1) ->
      Integer ->
      Type_1 ->
      (Map' Polykind, Map' Kind) ->
      Err ((Alg_pat_2, Typedexpr), Eqtns, (Integer, Integer), [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a c d (Eqtns e y' p q) f (g, h) i j k =
    (
      get_pattern_type c a (d, e, p, f) g (ntype (show i)) >>=
      \((m, n, s, t), (o, y)) -> (\(u, v, w, r0) -> ((o, u), v, w, r0, y)) <$> type_expression a c m (Eqtns n y' s q) t h j k)
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
  type_cat ::
    (
      (
        String,
        Map' Kind,
        Map' PConstructor,
        Map' Polykind,
        Map' Prom_alg,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' [[String]])) ->
      Cat_3 ->
      (Map' (Cat_4, Status), Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2) ->
      Err (Map' (Cat_4, Status), Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2))
  type_cat (a, o, t, u, a3, m', z') (Cat_3 b (Name c p, d) n (e, f, g, h, i)) (j, k, l, m) =
    let
      x = Prelude.foldl Application_kind_1 (Name_kind_1 p) (Name_kind_1 <$> d)
      y = Data.Map.union o (Data.Map.fromList ((\y' -> (y', Star_kind)) <$> d))
    in
      und_err
        p
        o
        "kind"
        (Location_1 a c)
        (\q ->
          case type_cat_help q d of
            LT -> Left ("Kind constructor " ++ p ++ location (Location_1 a c) ++ " has been given too few arguments.")
            EQ ->
              (
                type_cat_constrs (Location_1 a, Data.Set.fromList d) (fst <$> j, n) >>=
                \(r, s) ->
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
                                    f7 >>=
                                  \(k7, f0) ->
                                    let
                                      s' m7 fj u1 n2 =
                                        type_expr
                                          (m7 ++ " " ++ p ++ location' (Location_1 a b))
                                          fj
                                          (Location_1 a)
                                          (fst <$> k7, fst <$> f0)
                                          u1
                                          z'
                                          n2
                                          m'
                                          (t3, y)
                                          (fst <$> j)
                                          b
                                    in
                                      (
                                        (\j7 -> \j8 ->
                                          (
                                            ins_new p (Cat_4 d s) j,
                                            k7,
                                            f0,
                                            Data.Map.insert ("Id " ++ p) j7 (Data.Map.insert ("Compose " ++ p) j8 m))) <$>
                                        s'
                                          "Compose"
                                          (function_type
                                            (arrow_type x (ntype "0") (ntype "1"))
                                            (function_type
                                              (arrow_type x (ntype "2") (ntype "0"))
                                              (arrow_type x (ntype "2") (ntype "1"))))
                                          h
                                          3 <*>
                                        s' "Id" (arrow_type x (ntype "0") (ntype "0")) i 1))))))
            GT -> Left ("Kind constructor " ++ p ++ location (Location_1 a c) ++ " has been given too many arguments."))
  type_cat_help :: Kind -> [String] -> Ordering
  type_cat_help a b =
    case (a, b) of
      (Star_kind, []) -> EQ
      (Star_kind, _) -> GT
      (Arrow_kind _, []) -> LT
      (Arrow_kind c, _ : d) -> type_cat_help c d
  type_cats ::
    (
      (
        String,
        Map' Kind,
        Map' PConstructor,
        Map' Polykind,
        Map' Prom_alg,
        Map' ([String], Map' [(String, Nat)]),
        Map' (Map' [[String]])) ->
      [Cat_3] ->
      (Map' (Cat_4, Status), Map' (Constructor, Status), Types, Map' Expression_2) ->
      Err (Map' (Cat_4, Status), Map' (Constructor, Status), Map' (Type_2, Status), Map' Expression_2))
  type_cats a f b =
    case f of
      [] -> Right b
      c : d -> type_cat a c b >>= type_cats a d
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
                      type_methods_0 a e (Data.Map.insert c (pkind h) j) i2)))))
  type_class_1 ::
    (
      String ->
      Class_3 ->
      Map' ([String], [String], Kind_1) ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Type_2, Status), Map' (Class_4, Status)) ->
      Err (Map' (Type_2, Status), Map' (Class_4, Status)))
  type_class_1 a (Class_3 b g1 g7 (c, k) g' e) d f1 c4 (f0, f) =
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
          type_methods_1 a f1 e)
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
      \(r, (m, p, p', _)) -> (\(k, n) -> (n, k, m, p, p')) <$> type_classes_1 a r (fst <$> p) (fst <$> p') w (g, e))
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
    Map' ([String], [String], Kind_1) ->
    Map' Class_5 ->
    Map' Cat_4 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_classes_1 a b h i f c =
    case b of
      [] -> Right c
      d : e -> type_class_1 a d h i f c >>= type_classes_1 a e h i f
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
    Map' (Map' [String]) -> Constraint_0 -> (Map' Class_5, Map' Kind_1) -> String -> Err (Map' (Map' [String]))
  type_constraint_0 k (Constraint_0 (Name b c) _ (Name d e)) (f, g) j =
    und_err
      c
      f
      "class"
      (Location_1 j b)
      (\(Class_5 t3 m4 h y h') ->
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
          Nothing -> Left ("Undefined type variable " ++ e ++ location' (Location_1 j d)))
  type_constraint_1 :: Constraint_1 -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraint_1 (Constraint_1 c m e) a b =
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
      Class_4 _ _ _ f _ = b ! c
    in
      case f of
        Just (g, _) -> type_constraint_1 (Constraint_1 g [] e) d b
        Nothing -> d
  type_constraints_0 ::
    (
      Map' (Map' [String]) ->
      [Constraint_0] ->
      (Map' Class_5, Map' Kind_1, Map' Nat) ->
      String ->
      Err ([Constraint_1], [String], [(String, Nat)]))
  type_constraints_0 g a (f, t, u) h =
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
  type_data_1 a (b, c) (Data_2 d e m4 f) (i, k) =
    (
      type_kt_0 (Location_1 a) b e m4 >>=
      \(l, y) ->
        (
          (\(n, o) ->
            ((ins_new d (Polykind Nothing [] (Prelude.foldr arrow_kind star_kind (snd <$> l))) i, n), Data_3 d e l o)) <$>
          type_data_br_1 (a, y, c) f k))
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_3 ->
      Map' Polykind ->
      Map' Kind ->
      (Map' (Constructor, Status), Types) ->
      Err (Map' (Constructor, Status), Types))
  type_data_2 a (Data_3 b z c d) e f g =
    type_data_br_2
      a
      (Prelude.foldl (\n -> \n' -> Application_type_1 n (ntype n')) (ntype b) (fst <$> c))
      d
      (type_kinds c e)
      (Prelude.foldl (\m -> \x -> Data.Map.insert x Star_kind m) f z)
      g
      (z, first Name_tpat <$> c)
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
      ([String], [(TPat, Kind_1)]) ->
      String ->
      Map' Kind_1 ->
      Err (Map' (Constructor, Status), Types))
  type_data_br_2 a b c d e (f, g) (x, l) o w1 =
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
            type_forms a i d e)
        Branching_data_3 j h k -> type_branchings_1 a b j k (Data.Map.delete j d) e (f, g) (x, l) o (Data.Map.delete j w1) h
        Struct_data_3 i j ->
          (
            (\k ->
              (
                ins_new i (Constructor x l (snd <$> k) b [(i, fromIntegral (length j))]) f,
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
        Map' (Prom_alg, Status),
        Map' (PConstructor, Status)) ->
      Err
        (
          Map' (Polykind, Status),
          Map' (Constructor, Status),
          Types,
          Map' (Kind, Status),
          Map' Expression_2,
          Map' (Bool, Status),
          Map' (Prom_alg, Status),
          Map' (PConstructor, Status)))
  type_datas h a (b, i, d, o, c, x, a0, e1) =
    (
      type_proms_1 (Location_1 h) a (o, b, c) (make_eqs a (first Left <$> x)) >>=
      \((e, p, r), f, g, k) ->
        (
          type_proms_2 (Location_1 h) f (fst <$> o) (p, d, a0, i, e1) >>=
          \(t, n, b0, l, n3) ->
            (
              type_datas_1 h (fst <$> e, fst <$> b0) g (t, r) >>=
              \((u, w), y) ->
                (
                  (\(b', c') -> (u, b', c', e, w, first unsafe_left <$> k, b0, n3)) <$>
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
    (
      String ->
      Map' Kind ->
      Def_3 ->
      Map' Polykind ->
      Map' (Type_2, Status) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err (Def_4, Map' (Type_2, Status), (Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)]))))
  type_def_1 l x a b c k k2 w4 (t', u3) =
    case a of
      Basic_def_3 f d e e' g i ->
        (
          type_kt_1 (Location_1 l) ((x, b, Data.Map.empty, w4), e) >>=
          \((x2, j, j', u9), KT2 e0 m4 y) ->
            (
              type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') l >>=
                \(o1, o2, _) ->
                (
                  (\h ->
                    (
                      Basic_def_4 f d (KT2 e0 m4 y) o1 h i o2,
                      ins_new d (Type_2 Nothing e0 [] (first Name_tpat <$> y) Nothing o1 h) c,
                      (t', u3))) <$>
                  type_typ (Location_1 l) j x2 star_kind g)))
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
                \(w5, s3) ->
                  (
                    ziphelp (Location_1 l) x4 m e Data.Map.empty d2 k3 >>=
                    \(_, g9) ->
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
                                      type_constraints_0 Data.Map.empty o' (k2, t0, t7) l >>=
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
                                                  (case Data.Map.lookup m t' of
                                                    Just _ -> adjust (ins_new n r') m
                                                    Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New)))
                                                      t',
                                                  adjust (second (Data.Map.insert n o3)) m u3))) <$>
                                            type_cls_0 n (repkinds_method g9 <$> q) s' g (Location_1 l) m d)))))))))
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
    Map' Cat_4 ->
    Err (Map' Expression_2)
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
                      Just u ->
                        case constr_check u0 (fst <$> e0) u r' of
                          Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                          Nothing -> r
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
  type_defs ::
    (
      String ->
      Map' Kind ->
      [Def_3] ->
      [Name] ->
      (Map' Polykind, Map' Constructor) ->
      (Map' Expression_2, Types) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' Cat_4 ->
      (Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err (Map' Expression_2, Types, Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)])))
  type_defs h x a a2 (b, i) (c, d) y y0 w2 t =
    (
      type_defs_1 h x a b d y y0 w2 t >>=
      \(g, e, (u, f')) ->
        (
          (\f -> (f, e, u, f')) <$
          type_ops h (fst <$> e) a2 <*>
          type_defs_2 (Location_1 h) g (i, fst <$> e) c ((<$>) fst <$> u) b f' y x w2))
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
      (Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)])) ->
      Err ([Def_4], Types, (Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)]))))
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
    Map' (Map' [[String]]) ->
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
            (Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") (Name_kind_1 (show i))) k)
            (Data.Set.insert (show i) s, f) >>=
          \(i', f', b') ->
            (\(i2, f2, c') -> (i2, f2, Application_type_1 b' c')) <$> type_eqs m i' c d e (Name_kind_1 (show i)) f')
      Char_type_5 b -> Right (i, (s, (k, char_kind) : f), Char_type_1 b)
      Int_type_5 b -> Right (i, (s, (k, int_kind) : f), Int_type_1 b)
      Name_type_5 (Name b c) h g ->
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
              t u =
                Right
                  (
                    p,
                    (Data.Set.union s (Data.Set.fromList q), u ++ [(k, kindrep' (Data.Map.fromList (zip o x)) n)] ++ f),
                    case j of
                      Nothing -> Name_type_1 c Nothing x
                      Just _ -> Name_type_1 c (Just (head x)) (tail x))
            in
              case (j, h) of
                (Nothing, Nothing) ->
                  case g of
                    [] -> t []
                    _ ->
                      case compare (length g) (length l) of
                        LT -> Left ("Too few kind arguments for type " ++ c ++ location' (m b))
                        EQ -> traverse (type_kind_7 m e Star_kind) g >>= \v -> t (zip x v)
                        GT -> Left ("Too many kind arguments for type " ++ c ++ location' (m b))
                (Nothing, Just _) -> Left ("Illegal ad hoc kind argument for " ++ c ++ location' (m b))
                (Just _, Nothing) ->
                  case g of
                    [] -> t []
                    _ ->
                      case compare (length g) (length l) of
                        LT -> Left ("Too few kind arguments for type " ++ c ++ location' (m b))
                        EQ -> traverse (type_kind_7 m e Star_kind) g >>= \v -> t (zip (tail x) v)
                        GT -> Left ("Too many kind arguments for type " ++ c ++ location' (m b))
                (Just _, Just s') ->
                  (
                    type_kind_7 m e Star_kind s' >>=
                    \u -> 
                      case g of
                        [] -> t [(head x, u)]
                        _ ->
                          case compare (length g) (length l) of
                            LT -> Left ("Too few kind arguments for type " ++ c ++ location' (m b))
                            EQ -> traverse (type_kind_7 m e Star_kind) g >>= \v -> t (zip x (u : v))
                            GT -> Left ("Too many kind arguments for type " ++ c ++ location' (m b))))
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
    Map' Cat_4 ->
    Location_0 ->
    Err Expression_2
  type_expr k h a (c, e) f m w w' b a3 l3 =
    let
      n = " in " ++ k
    in
      (
        type_expression c a (0, w) (Eqtns (Data.Set.empty, Data.Set.empty) [] [] []) e f h b >>=
        \(g, (Eqtns i q8 j x), _, x3) ->
          (
            solvesys (\y -> \p -> Left ("Type mismatch between " ++ min y p ++ " and " ++ max y p ++ n)) j (x, g, i) >>=
            \(y, p, (k2, k')) ->
              case Data.Set.null k2 of
                False -> Left ("Unresolved kind variables" ++ n)
                True ->
                  case Data.Set.null k' of
                    False -> Left ("Unresolved type variables" ++ n)
                    True ->
                      (
                        addargs w' p <$
                        traverse_ (check_cat (a l3) a3) q8 <*
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
    (Map' Polykind, Map' Constructor, Map' Type_2, Map' Kind) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Cat_4 ->
    Err Expression_2
  type_expr' (b, c, e, i) f g h c3 =
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
  type_expression v r (o', o) (Eqtns (f4, f) mi h c') d b e (r7, m8) =
    let
      x' a = location' (r a)
    in
      case b of
        Application_expression_1 c g ->
          (
            type_expression
              v
              r
              (o', o + 1)
              (Eqtns (f4, Data.Set.insert (show o) f) mi h c')
              d
              c
              (function_type (ntype (show o)) e)
              (r7, m8) >>=
            \(i, j, p, d7) ->
              (
                (\(l, m, q, e') -> (Application_texpr i l, m, q, d7 ++ e')) <$>
                type_expression v r p j d g (ntype (show o)) (r7, m8)))
        Char_expression_1 c -> Right (Char_texpr c, Eqtns (f4, f) mi ((e, char_type) : h) c', (o', o), [])
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
                  (Eqtns (f5, Data.Set.insert (show c6) d6) mi ((e, function_type (ntype (show o)) (ntype (show c6))) : f6) c')
                  b6
                  g
                  (ntype (show c6))
                  (r7, m8)))
        Int_expression_1 c -> Right (Int_texpr c, Eqtns (f4, f) mi ((e, int_type) : h) c', (o', o), [])
        Match_expression_1 a7 c g ->
          (
            type_expression v r (o', o + 1) (Eqtns (f4, Data.Set.insert (show o) f) mi h c') d c (ntype (show o)) (r7, m8) >>=
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
              (\(Type_2 m2 i3 m3 i x0 a' j) ->
                let
                  a8 = maybeToList m2 ++ i3
                  o9 = o' + fromIntegral (length a8)
                  f6 = show <$> [o' .. o9 - 1]
                  d1 = Data.Map.fromList (zip a8 (Name_kind_1 <$> f6))
                  i4 = second (repkinds d1) <$> i
                  (f0, p, kl) = typevars_0 i4 (o, Data.Map.empty, f)
                  x7 = (\(Constraint_1 a0 _ b0) -> (a0, (Name a7 c, p ! b0))) <$> a'
                  g7 k2 e4 =
                    (
                      k2,
                      Eqtns
                        (Data.Set.union f4 (Data.Set.fromList f6), kl)
                        ((Name_kind_1 <$> m3) ++ mi)
                        (e4 ++ [(e, repl' p (repkinds_type d1 j))] ++ h)
                        (x7 ++ c'),
                      (o9, f0),
                      [])
                in
                  case (g, x0) of
                    (Nothing, Nothing) ->
                      (
                        g7 (Name_texpr_1 c (second snd <$> x7)) <$>
                        case k of
                          [] -> Right []
                          _ ->
                            case compare (length k) (length i4) of
                              LT -> e5 "few"
                              EQ ->
                                zip (ntype <$> show <$> [o ..]) <$> traverse (uncurry (type_typ r r7 m8)) (zip (snd <$> i4) k)
                              GT -> e5 "many")
                    (Nothing, Just (Constraint_1 y0 _ _)) ->
                      (
                        g7 (Name_texpr_0 c y0 (Name_type_1 (show o) Nothing [])) <$>
                        case k of
                          [] -> Right []
                          _ ->
                            case compare (length k) (length (tail i4)) of
                              LT -> e5 "few"
                              EQ ->
                                (
                                  zip (ntype <$> show <$> [o + 1 ..]) <$>
                                  traverse (uncurry (type_typ r r7 m8)) (zip (snd <$> tail i4) k))
                              GT -> e5 "many")
                    (Just _, Nothing) -> Left ("Invalid class argument for variable " ++ c ++ x' a7)
                    (Just t3, Just (Constraint_1 y0 _ _)) ->
                      (
                        g7 (Name_texpr_0 c y0 (Name_type_1 (show o) Nothing [])) <$>
                        case k of
                          [] -> (\g0 -> [(ntype (show o), g0)]) <$> type_typ r r7 m8 (snd (head i4)) t3
                          _ ->
                            case compare (length k) (length (tail i4)) of
                              LT -> e5 "few"
                              EQ ->
                                (
                                  zip (ntype <$> show <$> [o ..]) <$>
                                  traverse (uncurry (type_typ r r7 m8)) (zip (snd <$> i4) (t3 : k)))
                              GT -> e5 "many"))
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
  type_field :: (Location_0 -> Location_1) -> (String, Type_8) -> Map' Polykind -> Map' Kind -> Err (String, Type_1)
  type_field d (a, b) c e  = (,) a <$> type_typ d c e star_kind b
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
  type_kt_0 ::
    (Location_0 -> Location_1) -> Map' Kind -> [String] -> [(String, Kind_0)] -> Err ([(String, Kind_1)], Map' Kind_1)
  type_kt_0 a d b c = type_kinds_5 a (Prelude.foldl (\e -> \f -> Data.Map.insert f Star_kind e) d b) (c, Data.Map.empty)
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
  type_method :: (Location_0 -> Location_1) -> Method_2 -> Map' Polykind -> Map' Kind -> Err Method_3
  type_method a (Method_2 b c i d) e f = type_kinds_0 a f c e >>= \(g, h) -> Method_3 b g i <$> type_typ a h f star_kind d
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
          (\(Type_2 _ _ _ _ _ _ g) ->
            case g of
              Application_type_1
                (Application_type_1 (Name_type_1 "Arrow" (Just (Name_kind_1 "Star")) []) _)
                (Application_type_1 (Application_type_1 (Name_type_1 "Arrow" (Just (Name_kind_1 "Star")) []) _) _) ->
                  type_ops a b f
              _ -> Left ("Function " ++ e ++ location (Location_1 a d) ++ " takes less than 2 arguments."))
  type_pat ::
    (
      (Location_0 -> Location_1) ->
      Map' Constructor ->
      Pat' ->
      Type_1 ->
      Map' Type_2 ->
      (Integer, Integer) ->
      (Set String, Set String) ->
      [(Type_1, Type_1)] ->
      Err (Pat_1, Map' Type_2, (Integer, Integer), (Set String, Set String), [(Type_1, Type_1)]))
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
                    type_pats a b k (repl' r <$> n) e q s ((d, repl' r o) : h) (Name i j))
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
      [(Type_1, Type_1)] ->
      Name ->
      Err ([Pat_1], Map' Type_2, (Integer, Integer), (Set String, Set String), [(Type_1, Type_1)]))
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
  type_prom_1 a (Data_2 b y c d) (e, f, h) k =
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
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)))
  type_prom_2 f (Plain_dat a b c) y' (d, e, a0, k7, t9) =
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
                ins_new m3 (Constructor [] g2 (snd <$> i) x [(m3, fromIntegral (length i))]) k7,
                ins_new m3 (PConstructor b (prom_type <$> snd <$> i) (prom_type x) [(m3, fromIntegral (length i))]) t9)) <$>
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
      (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)) ->
      Err (Map' (Polykind, Status), Types, Map' (Prom_alg, Status), Map' (Constructor, Status), Map' (PConstructor, Status)))
  type_proms_2 a b y d =
    case b of
      [] -> Right d
      e : f -> type_prom_2 a e y d >>= type_proms_2 a f y
  type_rep :: (Kind_1 -> Kind_1) -> Type_1 -> Type_1
  type_rep a c =
    case c of
      Application_type_1 d e -> Application_type_1 (type_rep a d) (type_rep a e)
      Name_type_1 d e f -> Name_type_1 d (a <$> e) (a <$> f)
      _ -> c
  type_reps ::
    Location_1 -> Set String -> String -> Kind_1 -> [(Kind_1, Kind_1)] -> (Map' Kind_1, Type_1) -> Err (Map' Kind_1, Type_1)
  type_reps e a b c d (k, l) =
    let
      f = kindrep b c
    in
      solve_type_eqs e (Data.Set.delete b a) (bimap f f <$> d) (f <$> k, type_rep f l)
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
  type_typ :: (Location_0 -> Location_1) -> Map' Polykind -> Map' Kind -> Kind_1 -> Type_8 -> Err Type_1
  type_typ a d e f (Type_8 b c) =
    type_eqs a 0 c d e f (Data.Set.empty, []) >>= \(_, (g, h), i) -> snd <$> solve_type_eqs (a b) g h (Data.Map.empty, i)
  type_types :: (Location_0 -> Location_1) -> [Type_8] -> Map' Polykind -> Map' Kind -> Err [Type_1]
  type_types f a b g =
    case a of
      [] -> Right []
      c : d -> type_typ f b g star_kind c >>= \e -> (:) e <$> type_types f d b g
  type_types' :: (Location_0 -> Location_1) -> (Map' Kind, Map' Polykind) -> [(String, Type_8)] -> Err [(String, Type_1)]
  type_types' a (b, c) d =
    case d of
      [] -> Right []
      (e, f) : g -> type_typ a c b star_kind f >>= \h -> (:) (e, h) <$> type_types' a (b, c) g
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
      Char_type_1 b -> (show b, d)
      Int_type_1 b -> ('!' : show b, d)
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
  typing k (Tree_5 a m2 a' x7 c) (File d t v w w0 b' c5 x t2 u0 a4 k9, l, n4) =
    (
      type_datas k a (old d, old t, old v, old w, l, old w0, old u0, old k9) >>=
      \(e, b, g, o, f, w1, u1, t4) ->
        (
          type_cats (k, fst <$> o, fst <$> t4, fst <$> e, fst <$> u1, n4, x) m2 (old a4, b, g, f) >>=
          \(d5, t8, n7, a2) ->
            (
              type_classes k (fst <$> o) (fst <$> e) a' (fst <$> d5) (old b', n7, n4, old t2, old c5) >>=
              \(c', g0, x1, x2, t3) ->
                (
                  (\(i, j, y, y2) ->
                    (
                      File
                        (rem_old e)
                        (rem_old t8)
                        (rem_old j)
                        (rem_old o)
                        (rem_old w1)
                        (rem_old c')
                        (rem_old t3)
                        (rem_old' y)
                        (rem_old x2)
                        (rem_old u1)
                        (rem_old d5)
                        (rem_old t4),
                      i,
                      y2)) <$>
                  type_defs
                    k
                    (fst <$> o)
                    c
                    x7
                    (fst <$> e, fst <$> t8)
                    (a2, g0)
                    (fst <$> c')
                    (fst <$> t3)
                    (fst <$> d5)
                    (old' x, x1)))))
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