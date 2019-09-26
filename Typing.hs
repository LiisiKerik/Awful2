{-
Jaskelioff "Modular Monad Transformers" - saada need naited toole Awfulis
Lugeda tyybiperede kohta
Hargnevate andmetyypide eelised? Seosed tyybiperedega?
Klasside ja liikide vordsus - mis on seos Scala subtypinguga?
pattern matching in types
generalise branching types to branch not only over promoted algebraics but over Star, Int and Char
liigirakendamise eemaldamine liigituletuse kasuks (igal pool? teatud piiratud juhtudel?)
stringide syntaktiline suhkur
piiratud pesastatud mustrid hargnevas andmetüübis, nt: Construct_List (Mk_Pair T U) L ->
All Ord C, All c C <- klassimuutuja
polütüüpsus
pattern match types with only one type constructor?
promote constructor operators (two type ops - for pair and either - and also promotable data constructor operators)
improve cat syntax
viia "kas asi on tõesti tüübikonstruktor?" kontroll tüüpijasse
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Classes
  import Data.Map
  import Datas
  import Defs
  import Naming
  import Standard
  import Tokenise
  import Tree
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
  context_union :: (File, Map' Op) -> (File, Map' Op) -> (File, Map' Op)
  context_union (File i j d a x e q t g o z w', t0) (File k l h c y m r u n p p2 a', t2) =
    (
      File
        (union i k)
        (union j l)
        (union d h)
        (union a c)
        (union x y)
        (union e m)
        (union q r)
        (unionWith union t u)
        (union g n)
        (union o p)
        (union z p2)
        (union w' a'),
      union t0 t2)
  defs :: Map' Expression_2
  defs =
    fromList
      [
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
        ("Next", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Next" [Name_expression_2 "x"])),
        ("Nothing", Algebraic_expression_2 "Nothing" []),
        ("Right", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Right" [Name_expression_2 "x"])),
        ("Second", Field_expression_2 1),
        ("Write_Brackets Int", Write_Brackets_Int_expression_2),
        ("Zero", Algebraic_expression_2 "Zero" []),
        ("add Int", Add_Int_0_expression_2),
        ("compare Char", Compare_Char_0_expression_2),
        ("compare Int", Compare_Int_0_expression_2),
        (
          "compose Star",
          Function_expression_2
            (Name_pat_1 "f")
            (Function_expression_2
              (Name_pat_1 "g")
              (Function_expression_2
                (Name_pat_1 "x")
                (Application_expression_2
                  (Name_expression_2 "f")
                  (Application_expression_2 (Name_expression_2 "g") (Name_expression_2 "x")))))),
        ("convert Int", Convert_Int_expression_2),
        ("div", Div_0_expression_2),
        ("id Star", Function_expression_2 (Name_pat_1 "x") (Name_expression_2 "x")),
        ("mod", Mod_0_expression_2),
        ("multiply Int", Multiply_Int_0_expression_2),
        ("negate Int", Negate_Int_expression_2)]
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
        (singleton "Star" (Cat_4 [] []))
        pconstrs,
      empty)
  locations :: Locations
  locations =
    fromList
      (
        (\x -> (x, Language)) <$>
        [
          "Arrow",
          "Char",
          "ConstructList",
          "EQ",
          "Either",
          "EmptyList",
          "GT",
          "Int",
          "Just",
          "LT",
          "Left",
          "List",
          "Maybe",
          "MkPair",
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
          "compose",
          "convert",
          "div",
          "fst",
          "id",
          "mod",
          "multiply",
          "negate",
          "undefined"])
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
  type_expr' ::
    (
      (Map' Polykind, Map' PConstructor, Map' Constructor, Map' Type_2, Map' Kind, Map' Prom_alg) ->
      Expression_1 ->
      Map' (Map' Inst) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Cat_4 ->
      Err Expression_2)
  type_expr' (b, x, c, e, i, j2) f g h c3 =
    type_expr
      "input."
      (list_type char_type)
      "input"
      (j2, x, c, e)
      (Application_expression_1
        (Name_expression_1 (Name (Location_0 0 0) "First"))
        (Application_expression_1 (Name_expression_1 (Name (Location_0 0 0) "Write_Brackets")) f))
      g
      0
      h
      (b, i)
      c3
      (Location_0 0 0)
  types :: Map' Type_2
  types =
    fromList
      [
        (
          "compose",
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
          "id",
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
  typing ::
    String ->
    Tree_5 ->
    (File, Map' Expression_2, Map' ([String], Map' [(String, Nat)])) ->
    Err (File, Map' Expression_2, Map' ([String], Map' [(String, Nat)]))
  typing a (Tree_5 b c d f) (File g h i j k l m n o p q r, s, t) =
    (
      type_datas a (b, c) (g, h, i, j, k, p, q, r, s) >>=
      \((f', m', n', u, z, c', i', e', o'), p') ->
        (
          type_classes a (fst <$> u) (fst <$> f') d (fst <$> i') (old l, n', t, old o, old m) >>=
          \(q', r', s', t', u') ->
            (
              (\(w', x', b0, y') ->
                (
                  File
                    (rem_old f')
                    (rem_old m')
                    w'
                    (rem_old u)
                    z
                    (rem_old q')
                    (rem_old u')
                    x'
                    (rem_old t')
                    (rem_old c')
                    (rem_old i')
                    (rem_old e'),
                  b0,
                  y')) <$>
              type_defs
                a
                (fst <$> u, fst <$> f', fst <$> q', fst <$> u', fst <$> i', fst <$> m', fst <$> e', fst <$> c')
                (f, p')
                (r', n, o', s'))))
--------------------------------------------------------------------------------------------------------------------------------