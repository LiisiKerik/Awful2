--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Cats_0 (Cat_5 (..), type_cat_constrs, type_cats_0) where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Datas_0
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Cat_5 =
    Cat_5
      Location_0
      (String, [String])
      [String]
      ((Location_0, Patn'), (Location_0, Patn'), Data_br_2, Expression_1, Expression_1)
        deriving Show
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
--------------------------------------------------------------------------------------------------------------------------------