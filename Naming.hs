--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming where
  import Control.Monad.Trans.State.Strict
  import Data.Bifunctor
  import Data.Map
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_1 = Application_alg_pat_1 Name [Alg_pat_1] | Blank_alg_pat_1 | Int_alg_pat_1 Integer | Name_alg_pat_1 String
    deriving Show
  data Cat_2 =
    Cat_2 Location_0 (Name, [Name]) [Name] ((Location_0, Patn), (Location_0, Patn), Data_br_1, Expression_9, Expression_9)
      deriving Show
  data Cat_3 =
    Cat_3 Location_0 (Name, [String]) [Name] ((Location_0, Patn'), (Location_0, Patn'), Data_br_2, Expression_1, Expression_1)
      deriving Show
  data Class_1 = Class_1 String [Name] [Name] (Name, Kind_0) (Maybe Constraint_0) [Method_1] deriving Show
  data Class_2 = Class_2 String [String] [Name] (String, Kind_0) (Maybe Constraint_0) [Method_2] deriving Show
  data Data_1 = Data_1 String [(Name, Kind_0)] Data_br_1 deriving Show
  data Data_2 = Data_2 String [(String, Kind_0)] Data_br_2 deriving Show
  data Data_br_1 =
    Algebraic_data_1 [Form_1] | Branching_data_1 Location_0 Name [Data_case_1] | Struct_data_1 String [(String, Type_8)]
      deriving Show
  data Data_br_2 =
    Algebraic_data_2 [Form_1] | Branching_data_2 Location_0 Name [Data_case_2] | Struct_data_2 String [(String, Type_8)]
      deriving Show
  data Data_case_1 = Data_case_1 Name [Name] Data_br_1 deriving Show
  data Data_case_2 = Data_case_2 Name [String] Data_br_2 deriving Show
  data Def_2 =
    Basic_def_2 Location_0 String KT0 [Constraint_0] Type_8 Expression_9 |
    Instance_2
      Location_0
      [Name]
      [Name]
      Name
      [Kind_0]
      (Name, Maybe Kind_0, [Kind_0], [Pattern_1])
      [Constraint_0]
      [(Name, Expression_9)]
        deriving Show
  data Def_3 =
    Basic_def_3 Location_0 String KT1 [Constraint_0] Type_8 Expression_1 |
    Instance_3
      Location_0
      [String]
      [Name]
      Name
      [Kind_0]
      (Name, Maybe Kind_0, [Kind_0], [Pattern_0])
      [Constraint_0]
      [(Name, Expression_1)]
        deriving Show
  data Expression_1 =
    Application_expression_1 Expression_1 Expression_1 |
    Function_expression_1 Pat' Expression_1 |
    Int_expression_1 Integer |
    Match_expression_1 Location_0 Expression_1 [(Alg_pat_1, Expression_1)] |
    Name_expression_1 Name
      deriving Show
  data Form_1 = Form_1 String [Type_8] deriving Show
  data KT1 = KT1 [String] [Name] [(String, Kind_0)] deriving Show
  type Locations = Map' Location'
  data Method_1 = Method_1 String [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Method_2 = Method_2 String [(String, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Pat' = Application_pat' Name [Pat'] | Blank_pat' | Name_pat' String deriving Show
  data Patn' = Application_patn' Name [Patn'] | Name_patn' String deriving Show
  data Pattern_0 = Blank_pattern_0 | Name_pattern_0 String deriving Show
  data Tree_4 = Tree_4 [Data_1] [Cat_2] [Class_1] [Def_2] deriving Show
  data Tree_5 = Tree_5 [Data_2] [Cat_3] [Class_2] [Def_3] deriving Show
  add :: Ord t => Map t u -> t -> u -> Either u (Map t u)
  add x y z =
    let
      (w, x') = insertLookupWithKey (return return) y z x
    in
      case w of
        Just z' -> Left z'
        Nothing -> Right x'
  location_err :: String -> Location' -> Location_1 -> String
  location_err a c d =
    (
      "Conflicting " ++
      a ++
      (case c of
        Language -> " in the language"
        Library e -> location e) ++
      " and" ++
      location' d)
  naming ::
    (
      String ->
      Tree_2 ->
      (Locations, Locations, Locations, Map' (Map' Location')) ->
      Err ((Locations, Locations, Locations, Map' (Map' Location')), Tree_5))
  naming f a b = naming_1 f a b >>= \(d, (e, g, h, i)) -> (,) (e, g, h, i) <$> naming_2 f d e
  naming_1 ::
    (
      String ->
      Tree_2 ->
      (Locations, Locations, Locations, Map' (Map' Location')) ->
      Err (Tree_4, (Locations, Locations, Locations, Map' (Map' Location'))))
  naming_1 f (Tree_2 j a o g b) (k, l, r, v) =
    (
      naming_ops f l j >>=
      \m ->
        (
          runStateT (naming_datas_1 f a) k >>=
          \(e, s) ->
            (
              runStateT (naming_list naming_cat_0 f o) (s, r) >>=
              \(q, (d, u)) ->
                (
                  runStateT (naming_list naming_class_0 f g) d >>=
                  \(e', d') ->
                    bimap (Tree_4 e q e') (\(i, w) -> (i, m, u, w)) <$> runStateT (naming_list naming_def_1 f b) (d', v)))))
  naming_2 :: String -> Tree_4 -> Locations -> Err Tree_5
  naming_2 a (Tree_4 c b d f) h =
    Tree_5 <$> naming_datas_2 a c h <*> traverse (naming_cat_1 a h) b <*> naming_classes_1 a d h <*> naming_defs_2 a f h
  naming_alg_pattern :: String -> Alg_pat_7 -> StateT Locations Err Alg_pat_1
  naming_alg_pattern a d =
    case d of
      Application_alg_pat_7 g f -> Application_alg_pat_1 g <$> naming_list naming_alg_pattern a f
      Blank_alg_pat_7 -> return Blank_alg_pat_1
      Int_alg_pat_7 e -> return (Int_alg_pat_1 e)
      Name_alg_pat_7 b -> Name_alg_pat_1 <$> naming_name a b
  naming_args :: String -> [(Name, t)] -> Locations -> Err [(String, t)]
  naming_args a b c =
    case b of
      [] -> Right []
      (d, e) : f -> runStateT (naming_name a d) c >>= \(h, g) -> (:) (h, e) <$> naming_args a f g
  naming_argument :: (String -> t -> StateT Locations Err u) -> String -> (t, v) -> StateT Locations Err (u, v)
  naming_argument a e (b, c) = StateT (\d -> first (flip (,) c) <$> runStateT (a e b) d)
  naming_arguments :: (String -> t -> StateT Locations Err u) -> String -> [(t, v)] -> StateT Locations Err [(u, v)]
  naming_arguments a = naming_list (naming_argument a)
  naming_arguments' :: String -> (String -> t -> StateT Locations Err u) -> [(t, v)] -> Locations -> Err [(u, v)]
  naming_arguments' c a b = fmap fst <$> runStateT (naming_arguments a c b)
  naming_case :: String -> Locations -> (Alg_pat_7, Expression_9) -> Err (Alg_pat_1, Expression_1)
  naming_case a g (c, d) = runStateT (naming_alg_pattern a c) g >>= \(f, e) -> (,) f <$> naming_expression a d e
  naming_cat_0 :: String -> Cat_1 -> StateT (Locations, Locations) Err Cat_2
  naming_cat_0 a (Cat_1 c (Name d n, o) a' (e, f, g, h, i)) =
    StateT
      (\(k, l) ->
        case Data.Map.lookup n l of
          Nothing ->
            (
              bimap (\j -> Cat_2 c (Name d n, o) a' (e, f, j, h, i)) (\m -> (m, insert n (Library (Location_1 a c)) l)) <$>
              runStateT (naming_data_br_1 a g) k)
          Just j -> Left (location_err ("categories for kind " ++ n) j (Location_1 a c)))
  naming_cat_1 :: String -> Locations -> Cat_2 -> Err Cat_3
  naming_cat_1 a d (Cat_2 e (f, g) m2 ((v, h), (w, i), j, k, l)) =
    (
      runStateT (naming_names'' a g) d >>=
      \(m, n) ->
        (
          (\(o, p, q) -> \r -> \s -> Cat_3 e (f, m) m2 ((v, o), (w, p), q, r, s)) <$>
          (
            runStateT ((,) <$> naming_patn a h <*> naming_patn a i) n >>=
            \((p, r), q) -> (\s -> (p, r, s)) <$> naming_data_br_2 a j q) <*>
          naming_expression a k n <*>
          naming_expression a l n))
  naming_class_0 :: String -> Class_7 -> StateT Locations Err Class_1
  naming_class_0 a (Class_7 b i j c h d) = (\g -> Class_1 g i j c h) <$> naming_name a b <*> naming_list naming_method_0 a d
  naming_class_1 :: String -> Class_1 -> Locations -> Err Class_2
  naming_class_1 a (Class_1 b j m (c, d) h e) f =
    (
      runStateT (naming_name a c) f >>=
      \(g, i) -> runStateT (naming_names'' a j) i >>= \(k, l) -> Class_2 b k m (g, d) h <$> naming_methods_1 a e l)
  naming_classes_1 :: String -> [Class_1] -> Locations -> Err [Class_2]
  naming_classes_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_class_1 a d c >>= \g -> (:) g <$> naming_classes_1 a e c
  naming_data_1 :: String -> Data_6 -> StateT Locations Err Data_1
  naming_data_1 a (Data_6 b d e) = naming_name a b >>= \c -> Data_1 c d <$> naming_data_br_1 a e
  naming_data_2 :: String -> Data_1 -> Locations -> Err Data_2
  naming_data_2 a (Data_1 b h d) e =
    runStateT (naming_arguments naming_name a h) e >>= \(g, f) -> Data_2 b g <$> naming_data_br_2 a d f
  naming_data_br_1 :: String -> Data_br_6 -> StateT Locations Err Data_br_1
  naming_data_br_1 a b =
    case b of
      Algebraic_data_6 e -> Algebraic_data_1 <$> naming_forms a e
      Branching_data_6 c e f -> Branching_data_1 c e <$> naming_list naming_data_case_1 a f
      Struct_data_6 e f -> naming_name a e >>= \h -> Struct_data_1 h <$> naming_fields a f
  naming_data_br_2 :: String -> Data_br_1 -> Locations -> Err Data_br_2
  naming_data_br_2 a b c =
    case b of
      Algebraic_data_1 d -> Right (Algebraic_data_2 d)
      Branching_data_1 f d e -> Branching_data_2 f d <$> naming_data_cases_2 a e c
      Struct_data_1 d e -> Right (Struct_data_2 d e)
  naming_data_case_1 :: String -> Data_case_6 -> StateT Locations Err Data_case_1
  naming_data_case_1 a (Data_case_6 b c d) = Data_case_1 b c <$> naming_data_br_1 a d
  naming_data_case_2 :: String -> Data_case_1 -> Locations -> Err Data_case_2
  naming_data_case_2 a (Data_case_1 b c d) e =
    runStateT (naming_names'' a c) e >>= \(f, g) -> Data_case_2 b f <$> naming_data_br_2 a d g
  naming_data_cases_2 :: String -> [Data_case_1] -> Locations -> Err [Data_case_2]
  naming_data_cases_2 a b c =
    case b of
      [] -> Right []
      d : e -> (:) <$> naming_data_case_2 a d c <*> naming_data_cases_2 a e c
  naming_datas_1 :: String -> [Data_6] -> StateT Locations Err [Data_1]
  naming_datas_1 = naming_list naming_data_1
  naming_datas_2 :: String -> [Data_1] -> Locations -> Err [Data_2]
  naming_datas_2 f a b =
    case a of
      [] -> Right []
      c : d -> naming_data_2 f c b >>= \e -> (:) e <$> naming_datas_2 f d b
  naming_def_1 :: String -> Def_1 -> StateT (Locations, Map' (Map' Location')) Err Def_2
  naming_def_1 i a =
    StateT
      (\(g, k) ->
        case a of
          Basic_def_1 c @ (Name h j) b x d e -> (\(_, f) -> (Basic_def_2 h j b x d e, (f, k))) <$> runStateT (naming_name i c) g
          Instance_1 b x t1 (Name c l) d (Name f m, t, n, o) h e ->
            case Data.Map.lookup l k of
              Nothing ->
                Right
                  (
                    Instance_2 b x t1 (Name c l) d (Name f m, t, n, o) h e,
                    (g, insert l (singleton m (Library (Location_1 i b))) k))
              Just p ->
                case Data.Map.lookup m p of
                  Nothing ->
                    Right
                      (
                        Instance_2 b x t1 (Name c l) d (Name f m, t, n, o) h e,
                        (g, adjust (insert m (Library (Location_1 i b))) l k))
                  Just q -> Left (location_err ("instances of " ++ l ++ "{" ++ m ++ "}") q (Location_1 i b)))
  naming_def_2 :: String -> Def_2 -> Locations -> Err Def_3
  naming_def_2 j a b =
    case a of
      Basic_def_2 k c d t f g -> runStateT (naming_kt j d) b >>= \(i, h) -> Basic_def_3 k c i t f <$> naming_expression j g h
      Instance_2 f x t c n (d, w, l, g) k e ->
        (
          runStateT (naming_names'' j x) b >>=
          \(y, z) ->
            runStateT (naming_patterns j g) z >>= \(i, h) -> Instance_3 f y t c n (d, w, l, i) k <$> naming_nameexprs j h e)
  naming_defs_2 :: String -> [Def_2] -> Locations -> Err [Def_3]
  naming_defs_2 = naming_list' naming_def_2
  naming_expression :: String -> Expression_9 -> Locations -> Err Expression_1
  naming_expression g a b =
    case a of
      Application_expression_9 c d -> Application_expression_1 <$> naming_expression g c b <*> naming_expression g d b
      Function_expression_9 c d ->
        runStateT (naming_pat g c) b >>= \(f, e) -> Function_expression_1 f <$> naming_expression g d e
      Int_expression_9 c -> Right (Int_expression_1 c)
      Let_expression_9 c d e ->
        (
          runStateT (naming_pat g c) b >>=
          \(h, f) ->
            (
              (\i -> \j -> Application_expression_1 (Function_expression_1 h j) i) <$>
              naming_expression g d f <*>
              naming_expression g e f))
      Match_expression_9 h c d -> naming_expression g c b >>= \e -> Match_expression_1 h e <$> traverse (naming_case g b) d
      Name_expression_9 c -> Right (Name_expression_1 c)
  naming_fields :: String -> [(Name, Type_8)] -> StateT Locations Err [(String, Type_8)]
  naming_fields = naming_arguments naming_name
  naming_form :: String -> Form_6 -> StateT Locations Err Form_1
  naming_form d (Form_6 a b) = (\c -> Form_1 c b) <$> naming_name d a
  naming_forms :: String -> [Form_6] -> StateT Locations Err [Form_1]
  naming_forms = naming_list naming_form
  naming_kt :: String -> KT0 -> StateT Locations Err KT1
  naming_kt a (KT0 b g c) = naming_names'' a b >>= \e -> KT1 e g <$> naming_arguments naming_name a c
  naming_list :: (String -> t -> StateT u Err v) -> String -> [t] -> StateT u Err [v]
  naming_list a h b =
    case b of
      [] -> return []
      d : e -> (:) <$> a h d <*> naming_list a h e
  naming_list' :: (String -> t -> u -> Err v) -> String -> [t] -> u -> Err [v]
  naming_list' a g b c =
    case b of
      [] -> Right []
      d : e -> a g d c >>= \f -> (:) f <$> naming_list' a g e c
  naming_method_0 :: String -> Method_9 -> StateT Locations Err Method_1
  naming_method_0 a (Method_9 b c g d) = (\f -> Method_1 f c g d) <$> naming_name a b
  naming_method_1 :: String -> Method_1 -> Locations -> Err Method_2
  naming_method_1 a (Method_1 b c g d) e = (\f -> Method_2 b f g d) <$> naming_args a c e
  naming_methods_1 :: String -> [Method_1] -> Locations -> Err [Method_2]
  naming_methods_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_method_1 a d c >>= \g -> (:) g <$> naming_methods_1 a e c
  naming_name :: String -> Name -> StateT Locations Err String
  naming_name f (Name a c) =
    StateT
      (\d -> bimap (flip (location_err ("definitions of " ++ c)) (Location_1 f a)) ((,) c) (add d c (Library (Location_1 f a))))
  naming_nameexprs :: String -> Locations -> [(Name, Expression_9)] -> Err [(Name, Expression_1)]
  naming_nameexprs a b c =
    case c of
      [] -> Right []
      (d, e) : f -> naming_expression a e b >>= \g -> (:) (d, g) <$> naming_nameexprs a b f
  naming_names'' :: String -> [Name] -> StateT Locations Err [String]
  naming_names'' = naming_list naming_name
  naming_ops :: String -> Locations -> [Name] -> Err Locations
  naming_ops a b c =
    case c of
      [] -> Right b
      Name d e : i ->
        let
          h = Location_1 a d
        in
          case Data.Map.lookup e b of
            Just g -> Left (location_err ("definitions of operator " ++ e) g h)
            Nothing -> naming_ops a (insert e (Library h) b) i
  naming_pat :: String -> Pat_2 -> StateT Locations Err Pat'
  naming_pat a c =
    case c of
      Application_pat_2 b e -> Application_pat' b <$> naming_list naming_pat a e
      Blank_pat_2 -> return Blank_pat'
      Name_pat_2 b -> Name_pat' <$> naming_name a b
  naming_patn :: String -> Patn -> StateT Locations Err Patn'
  naming_patn a c =
    case c of
      Application_patn b e -> Application_patn' b <$> naming_list naming_patn a e
      Name_patn b -> Name_patn' <$> naming_name a b
  naming_pattern :: String -> Pattern_1 -> StateT Locations Err Pattern_0
  naming_pattern f c =
    case c of
      Blank_pattern_1 -> return Blank_pattern_0
      Name_pattern_1 a -> Name_pattern_0 <$> naming_name f a
  naming_patterns :: String -> [Pattern_1] -> StateT Locations Err [Pattern_0]
  naming_patterns = naming_list naming_pattern
--------------------------------------------------------------------------------------------------------------------------------