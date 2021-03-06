{-
pattern matching in types
generalise branching types to branch not only over promoted algebraics but over Star and Int
All Ord C, All c C <- klassimuutuja
pattern match types with only one type constructor?
promote constructor operators (two type ops - for pair and either - and also promotable data constructor operators)
improve cat syntax
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing (File (..), context_union, defs, init_type_context, standard_naming_typing, type_expr') where
  import Cats_0
  import Cats_1
  import Cats_2
  import Classes_0
  import Classes_2
  import Control.Monad.Trans.State.Strict
  import Data.Map
  import Datas_0
  import Datas_1
  import Datas_2
  import Defs_and_instances_0
  import Defs_and_instances_1
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
  nat_type :: Type_1
  nat_type = ntype "Nat"
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      (
        (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
        (File, Map' Op),
        Map' Expression_2,
        Map' ([String], Map' [(String, Nat)])) ->
      Err
        (
          (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
          (File, Map' Op),
          Map' Expression_2,
          Map' ([String], Map' [(String, Nat)])))
  standard_naming_typing f a (b, (c, t), g, w) =
    (
      runStateT (standard_1 (Location_1 f) a) t >>=
      \(n', v) -> naming f n' b >>= \(d, e) -> (\(h, i, n) -> (d, (h, v), i, n)) <$> typing f e (c, g, w))
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
      type_proms (Location_1 a) b q (j, g, s, k, i, p, h, r) >>=
      \((u, a', w, z, b', c', d', e'), y) ->
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
                      type_classes_0
                        (Location_1 a)
                        (fst <$> u)
                        (fst <$> f')
                        d
                        (fst <$> i')
                        (t, old o, old m, Data.Map.empty) >>=
                      \(r7, (s', t', u', _)) ->
                        (
                          type_classes_1 a r7 (fst <$> u) (fst <$> t') (fst <$> u') (fst <$> i') (n', old l) >>=
                          \(r', q') ->
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
                                  (\a0 -> \b0 ->
                                    (
                                      File
                                        (rem_old f')
                                        (rem_old m')
                                        (rem_old w')
                                        (rem_old u)
                                        z
                                        (rem_old q')
                                        (rem_old u')
                                        (rem_old' x')
                                        (rem_old t')
                                        (rem_old c')
                                        (rem_old i')
                                        (rem_old e'),
                                      Data.Map.union (Data.Map.union o' a0) b0,
                                      y')) <$>
                                  type_cats_2
                                    (
                                      a,
                                      fst <$> u,
                                      fst <$> f',
                                      y',
                                      fmap fst <$> x',
                                      fst <$> i',
                                      fst <$> e',
                                      fst <$> m',
                                      fst <$> w',
                                      fst <$> c')
                                    p' <*>
                                  type_defs_2
                                    a
                                    v'
                                    (fst <$> c', fst <$> e', fst <$> m', fst <$> w')
                                    (fmap fst <$> x')
                                    (fst <$> f')
                                    y'
                                    (fst <$> q')
                                    (fst <$> u)
                                    (fst <$> i')))))))))
--------------------------------------------------------------------------------------------------------------------------------