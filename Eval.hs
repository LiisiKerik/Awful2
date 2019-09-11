--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
  import Data.Map
  import Datas
  import Defs
  import Naming
  import Standard
  import Tokenise
  import Tree
  import Typing
  eval :: Map' Expression_2 -> Expression_2 -> String
  eval a b =
    case eval' a b of
      Just c -> tostr c
      Nothing -> "undefined"
  eval' :: Map' Expression_2 -> Expression_2 -> Maybe Expression_2
  eval' a c =
    case c of
      Application_expression_2 d e ->
        (
          eval' a d >>=
          \h ->
            (
              eval' a e >>=
              \j ->
                case h of
                  Add_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 l -> Just (Add_Int_1_expression_2 l)
                      _ -> undefined
                  Add_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 n -> Just (Int_expression_2 (k + n))
                      _ -> undefined
                  Compare_Char_0_expression_2 ->
                    case j of
                      Char_expression_2 k -> Just (Compare_Char_1_expression_2 k)
                      _ -> undefined
                  Compare_Char_1_expression_2 k ->
                    case j of
                      Char_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Compare_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Compare_Int_1_expression_2 k)
                      _ -> undefined
                  Compare_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Convert_Int_expression_2 -> Just j
                  Div_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Div_1_expression_2 k)
                      _ -> undefined
                  Div_1_expression_2 k ->
                    case j of
                      Int_expression_2 l ->
                        Just
                          (case l of
                            0 -> nothing_algebraic
                            _ -> wrap_algebraic (Int_expression_2 (div k l)))
                      _ -> undefined
                  Field_expression_2 k ->
                    case j of
                      Algebraic_expression_2 _ l -> Just (l !! fromIntegral k)
                      _ -> undefined
                  Function_expression_2 k l -> eval' a (subst_pat' k j l)
                  Mod_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Mod_1_expression_2 k)
                      _ -> undefined
                  Mod_1_expression_2 k ->
                    case j of
                      Int_expression_2 l ->
                        Just
                          (case l of
                            0 -> nothing_algebraic
                            _ -> wrap_algebraic (Int_expression_2 (mod k l)))
                      _ -> undefined
                  Multiply_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Multiply_Int_1_expression_2 k)
                      _ -> undefined
                  Multiply_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Int_expression_2 (k * l))
                      _ -> undefined
                  Negate_Int_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Int_expression_2 (- k))
                      _ -> undefined
                  Write_Brackets_Int_expression_2 ->
                    case j of
                      Int_expression_2 k ->
                        Just (pair_expression (list_expression (show k)) (Algebraic_expression_2 "False" []))
                      _ -> undefined
                  _ -> undefined))
      Match_expression_2 b d -> eval' a b >>= \e -> eval' a (eval_match e d)
      Name_expression_2 d -> Data.Map.lookup d a >>= eval' a
      _ -> Just c
  eval_match :: Expression_2 -> [(Alg_pat_2, Expression_2)] -> Expression_2
  eval_match a b =
    case b of
      [] -> undefined
      (c, d) : e ->
        case eval_pat a c d of
          Nothing -> eval_match a e
          Just g -> g
  eval_pat :: Expression_2 -> Alg_pat_2 -> Expression_2 -> Maybe Expression_2
  eval_pat a b c =
    case (a, b) of
      (Algebraic_expression_2 d e, Application_alg_pat_2 f g) ->
        case d == f of
          False -> Nothing
          True -> eval_pats (zip e g) c
      (Char_expression_2 d, Char_alg_pat_2 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (Int_expression_2 d, Int_alg_pat_2 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (_, Blank_alg_pat_2) -> Just c
      (_, Name_alg_pat_2 d) -> Just (subst_expr d c a)
      _ -> Nothing
  eval_pats :: [(Expression_2, Alg_pat_2)] -> Expression_2 -> Maybe Expression_2
  eval_pats a b =
    case a of
      [] -> Just b
      (c, d) : e -> eval_pat c d b >>= eval_pats e
  list_expression :: String -> Expression_2
  list_expression =
    Prelude.foldr
      (\x -> \y -> Algebraic_expression_2 "ConstructList" [Char_expression_2 x, y])
      (Algebraic_expression_2 "EmptyList" [])
  nothing_algebraic :: Expression_2
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  pair_expression :: Expression_2 -> Expression_2 -> Expression_2
  pair_expression x y = Algebraic_expression_2 "Mk_Pair" [x, y]
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Function_expression_2 d e -> Function_expression_2 d (if subst_pat a d then e else f e)
        Match_expression_2 d e ->
          Match_expression_2 (f d) ((\(g, h) -> (g, if subst_help a g then h else f h)) <$> e)
        Name_expression_2 d -> if d == a then c else b
        _ -> b
  subst_help :: String -> Alg_pat_2 -> Bool
  subst_help a b =
    case b of
      Application_alg_pat_2 _ c -> or (subst_help a <$> c)
      Name_alg_pat_2 c -> c == a
      _ -> False
  subst_pat :: String -> Pat_1 -> Bool
  subst_pat a b =
    case b of
      Application_pat_1 c -> or (subst_pat a <$> c)
      Blank_pat_1 -> False
      Name_pat_1 c -> c == a
  subst_pat' :: Pat_1 -> Expression_2 -> Expression_2 -> Expression_2
  subst_pat' a b c =
    case a of
      Application_pat_1 d ->
        case b of
          Algebraic_expression_2 _ e -> subst_pats (zip d e) c
          _ -> undefined
      Blank_pat_1 -> c
      Name_pat_1 d -> subst_expr d c b
  subst_pats :: [(Pat_1, Expression_2)] -> Expression_2 -> Expression_2
  subst_pats a b =
    case a of
      [] -> b
      (c, d) : e -> subst_pats e (subst_pat' c d b)
  tokenise_parse_naming_typing_eval ::
    Locations ->
    Map' Polykind ->
    (Map' Prom_alg, Map' PConstructor, Map' Constructor, Map' Type_2) ->
    Map' Expression_2 ->
    String ->
    Map' (Map' Inst) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Kind ->
    Map' Op ->
    Map' Cat_4 ->
    Err String
  tokenise_parse_naming_typing_eval c f (d2, a', a, i) l b u v w q w' =
    (
      parse_expression b >>=
      \e ->
        (
          std_expr (Location_1 "input") q e >>=
          \e' -> naming_expression "input" e' c >>= \j -> eval l <$> type_expr' (f, a', a, i, w, d2) j u v w'))
  tostr :: Expression_2 -> String
  tostr x =
    case x of
      Algebraic_expression_2 "EmptyList" [] -> []
      Algebraic_expression_2 "ConstructList" [Char_expression_2 y, z] -> y : tostr z
      _ -> undefined
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Algebraic_expression_2 "Just" [x]
--------------------------------------------------------------------------------------------------------------------------------