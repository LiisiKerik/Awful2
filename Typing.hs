{-
Jaskelioff "Modular Monad Transformers" - saada need naited toole Awfulis
Lugeda tyybiperede kohta
Hargnevate andmetyypide eelised? Seosed tyybiperedega?
Klasside ja liikide vordsus - mis on seos Scala subtypinguga?
pattern matching in types
generalise branching types to branch not only over promoted algebraics but over Star and Int
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
        ("EQ", Algebraic_expression_2 "EQ" []),
        ("GT", Algebraic_expression_2 "GT" []),
        ("LT", Algebraic_expression_2 "LT" []),
        ("Next", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Next" [Name_expression_2 "x"])),
        ("Zero", Algebraic_expression_2 "Zero" []),
        ("add Int", Add_Int_0_expression_2),
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
        ("times Int", Multiply_Int_0_expression_2)]
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
          "EQ",
          "GT",
          "Int",
          "LT",
          "Nat",
          "Next",
          "Ord",
          "Ordering",
          "Ring",
          "Star",
          "Zero",
          "add",
          "compare",
          "compose",
          "convert",
          "div",
          "id",
          "mod",
          "times",
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
    type_expr "input." (ntype "0") "input" (j2, x, c, e) f g 1 h (b, i) c3 (Location_0 0 0)
  types :: Map' Type_2
  types =
    fromList
      [
        ("EQ", Type_2 Nothing [] [] [] Nothing [] comparison_type),
        ("GT", Type_2 Nothing [] [] [] Nothing [] comparison_type),
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
          "id",
          Type_2
            (Just "K")
            []
            []
            [(Name_tpat "T", Name_kind_1 "K")]
            Nothing
            []
            (arrow_type (Name_kind_1 "K") (ntype "T") (ntype "T"))),
        ("LT", Type_2 Nothing [] [] [] Nothing [] comparison_type),
        ("Next", Type_2 Nothing [] [] [] Nothing [] (function_type nat_type nat_type)),
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
        ("div", Type_2 Nothing [] [] [] Nothing [] (function_type int_type (function_type int_type int_type))),
        ("mod", Type_2 Nothing [] [] [] Nothing [] (function_type int_type (function_type int_type int_type))),
        (
          "times",
          Type_2
            Nothing
            []
            []
            [(Name_tpat "T", star_kind)]
            (Just (Constraint_1 "Ring" [] "T"))
            [Constraint_1 "Ring" [] "T"]
            (function_type (ntype "T") (function_type (ntype "T") (ntype "T")))),
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