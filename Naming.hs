-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_1 =
    Application_alg_pat_1 Location_0 String [Alg_pat_1] |
    Blank_alg_pat_1 |
    Char_alg_pat_1 Char |
    Int_alg_pat_1 Integer |
    Name_alg_pat_1 String
      deriving Show
  data Cat_2 = Cat_2 Location_0 (Name, [Name]) (Name, Name, Either Type_8 Data_br_1, Expression_9, Expression_9) deriving Show
  data Cat_3 = Cat_3 Location_0 (Name, [String]) (String, String, Either Type_8 Data_br_2, Expression_1, Expression_1)
    deriving Show
  data Class_1 = Class_1 String [Name] (Name, Kind_0) (Maybe Name) [Method_1] deriving Show
  data Class_2 = Class_2 String [String] (String, Kind_0) (Maybe Name) [Method_2] deriving Show
  data Data_1 = Data_1 String KT0 Data_br_1 deriving Show
  data Data_2 = Data_2 String KT1 Data_br_2 deriving Show
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
    Instance_2 Location_0 Name [Kind_0] (Name, [Kind_0], [Pattern_1]) [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Def_3 =
    Basic_def_3 Location_0 String KT1 [Constraint_0] Type_8 Expression_1 |
    Instance_3 Location_0 Name [Kind_0] (Name, [Kind_0], [Pattern_0]) [Constraint_0] [(Name, Expression_1)]
      deriving Show
  data Expression_1 =
    Application_expression_1 Expression_1 Expression_1 |
    Char_expression_1 Char |
    Function_expression_1 Pat Expression_1 |
    Int_expression_1 Integer |
    Match_expression_1 Location_0 Expression_1 [(Alg_pat_1, Expression_1)] |
    Name_expression_1 Name (Maybe Type_8) [Type_8]
      deriving Show
  data Form_1 = Form_1 String [Type_8] deriving Show
  data KT1 = KT1 [String] [(String, Kind_0)] deriving Show
  type Locations = Map' Location'
  data Match_Algebraic_1 = Match_Algebraic_1 Name [Pat] Expression_1 deriving Show
  data Match_char_1 = Match_char_1 Location_0 Char Expression_1 deriving Show
  data Match_Int_1 = Match_Int_1 Location_0 Integer Expression_1 deriving Show
  data Matches_1 =
    Matches_Algebraic_1 [Match_Algebraic_1] (Maybe (Location_0, Expression_1)) |
    Matches_char_1 [Match_char_1] Expression_1 |
    Matches_Int_1 [Match_Int_1] Expression_1
      deriving Show
  data Method_1 = Method_1 String [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Method_2 = Method_2 String [(String, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Tree_4 = Tree_4 [Cat_2] [Data_1] [Class_1] [Name] [Def_2] deriving Show
  data Tree_5 = Tree_5 [Cat_3] [Data_2] [Class_2] [Name] [Def_3] deriving Show
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
      (Set String, Locations, Locations, Locations, Map' (Map' Location')) ->
      Err ((Set String, Locations, Locations, Locations, Map' (Map' Location')), Tree_5))
  naming f a b = naming_1 f a b >>= \((c, e, g, h, i), d) -> (,) (c, e, g, h, i) <$> naming_2 f d (c, e)
  naming_1 ::
    (
      String ->
      Tree_2 ->
      (Set String, Locations, Locations, Locations, Map' (Map' Location')) ->
      Err ((Set String, Locations, Locations, Locations, Map' (Map' Location')), Tree_4))
  naming_1 f (Tree_2 o a g j b) (c, k, l, r, v) =
    (
      naming_cats_0 f ((c, k, r), o) >>=
      \((p, s, u), q) ->
      (
        naming_datas_1 f a (p, s) >>=
        \((t, d), e) ->
          (
            naming_classes_0 f g d >>=
            \(d', e') ->
              (
                (\(m, n) -> \(h, (i, w)) -> ((t, i, m, u, w), Tree_4 q e e' n h)) <$>
                naming_ops f l j <*>
                naming_defs_1 f b (d', v)))))
  naming_2 :: String -> Tree_4 -> (Set String, Locations) -> Err Tree_5
  naming_2 a (Tree_4 b c d e f) (g, h) =
    (
      Tree_5 <$> traverse (naming_cat_1 a (g, h)) b <*>
      naming_datas_2 a c h <*>
      naming_classes_1 a d h <*>
      Right e <*>
      naming_defs_2 a f (g, h))
  naming_alg_pats :: String -> (Set String, Locations) -> [Alg_pat] -> Err (Locations, [Alg_pat_1])
  naming_alg_pats a (b, c) d =
    case d of
      [] -> Right (c, [])
      e : f -> naming_alg_pattern a (b, c) e >>= \(g, h) -> second ((:) h) <$> naming_alg_pats a (b, g) f
  naming_alg_pattern :: String -> (Set String, Locations) -> Alg_pat -> Err (Locations, Alg_pat_1)
  naming_alg_pattern a (b, c) d =
    case d of
      Application_alg_pat g e f -> second (Application_alg_pat_1 g e) <$> naming_alg_pats a (b, c) f
      Blank_alg_pat -> Right (c, Blank_alg_pat_1)
      Char_alg_pat e -> Right (c, Char_alg_pat_1 e)
      Int_alg_pat e -> Right (c, Int_alg_pat_1 e)
      Name_alg_pat (Name e f) ->
        case Data.Set.member f b of
          False -> (\(g, _) -> (g, Name_alg_pat_1 f)) <$> naming_name a (Name e f) c
          True -> Right (c, Application_alg_pat_1 e f [])
  naming_application :: String -> (Set String, Locations) -> Err Expression_1 -> Expression_9 -> Err Expression_1
  naming_application a b c d = c >>= \e -> Application_expression_1 e <$> naming_expression a d b
  naming_args :: String -> [(Name, t)] -> Locations -> Err [(String, t)]
  naming_args a b c =
    case b of
      [] -> Right []
      (d, e) : f -> naming_name a d c >>= \(g, h) -> (:) (h, e) <$> naming_args a f g
  naming_argument ::
    (String -> t -> Locations -> Err (Locations, u)) -> String -> (t, v) -> Locations -> Err (Locations, (u, v))
  naming_argument a e (b, c) d = second (flip (,) c) <$> a e b d
  naming_arguments ::
    (String -> t -> Locations -> Err (Locations, u)) -> String -> [(t, v)] -> Locations -> Err (Locations, [(u, v)])
  naming_arguments = naming_list <$> naming_argument
  naming_arguments' :: String -> (String -> t -> Locations -> Err (Locations, u)) -> [(t, v)] -> Locations -> Err [(u, v)]
  naming_arguments' c a b = (<$>) snd <$> naming_arguments a c b
  naming_case :: String -> (Set String, Locations) -> (Alg_pat, Expression_9) -> Err (Alg_pat_1, Expression_1)
  naming_case a (b, g) (c, d) = naming_alg_pattern a (b, g) c >>= \(e, f) -> (,) f <$> naming_expression a d (b, e)
  naming_cat_0 :: String -> ((Set String, Locations, Locations), Cat_1) -> Err ((Set String, Locations, Locations), Cat_2)
  naming_cat_0 a ((b, k, l), Cat_1 c (Name d n, o) (e, f, g, h, i)) =
    case Data.Map.lookup n l of
      Nothing ->
        (
          bimap
            (\(j, m) -> (j, m, Data.Map.insert n (Library (Location_1 a c)) l))
            (\j -> Cat_2 c (Name d n, o) (e, f, j, h, i)) <$>
          case g of
            Left j -> Right ((b, k), Left j)
            Right j -> second Right <$> naming_data_br_1 a j (b, k))
      Just j -> Left (location_err ("categories for kind " ++ n) j (Location_1 a c))
  naming_cat_1 :: String -> (Set String, Locations) -> Cat_2 -> Err Cat_3
  naming_cat_1 a (b, d) (Cat_2 e (f, g) (h, i, j, k, l)) =
    (
      naming_names'' a g d >>=
      \(m, n) ->
        (
          (\(o, p, q) -> \r -> \s -> Cat_3 e (f, m) (o, p, q, r, s)) <$>
          (
            naming_name a h n >>=
            \(o, p) ->
              (
                naming_name a i o >>=
                \(q, r) ->
                  (
                    (\s -> (p, r, s)) <$>
                    case j of
                      Left s -> Right (Left s)
                      Right s -> Right <$> naming_data_br_2 a s q))) <*>
          naming_expression a k (b, n) <*>
          naming_expression a l (b, n)))
  naming_cats_0 :: String -> ((Set String, Locations, Locations), [Cat_1]) -> Err ((Set String, Locations, Locations), [Cat_2])
  naming_cats_0 a (b, c) =
    case c of
      [] -> Right (b, [])
      d : e -> naming_cat_0 a (b, d) >>= \(f, g) -> second ((:) g) <$> naming_cats_0 a (f, e)
  naming_class_0 :: String -> Class_7 -> Locations -> Err (Locations, Class_1)
  naming_class_0 a (Class_7 b i c h d) e = naming_name a b e >>= \(f, g) -> second (Class_1 g i c h) <$> naming_methods_0 a d f
  naming_class_1 :: String -> Class_1 -> Locations -> Err Class_2
  naming_class_1 a (Class_1 b j (c, d) h e) f =
    naming_name a c f >>= \(i, g) -> naming_names'' a j i >>= \(k, l) -> Class_2 b k (g, d) h <$> naming_methods_1 a e l
  naming_classes_0 :: String -> [Class_7] -> Locations -> Err (Locations, [Class_1])
  naming_classes_0 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_class_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_classes_0 a e f
  naming_classes_1 :: String -> [Class_1] -> Locations -> Err [Class_2]
  naming_classes_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_class_1 a d c >>= \g -> (:) g <$> naming_classes_1 a e c
  naming_data_1 :: String -> Data_6 -> (Set String, Locations) -> Err ((Set String, Locations), Data_1)
  naming_data_1 a (Data_6 b c d e) (f, g) =
    naming_name a (Name b c) g >>= \(h, _) -> second (Data_1 c d) <$> naming_data_br_1 a e (f, h)
  naming_data_2 :: String -> Data_1 -> Locations -> Err Data_2
  naming_data_2 a (Data_1 b c d) e = naming_kt a c e >>= \(f, g) -> Data_2 b g <$> naming_data_br_2 a d f
  naming_data_br_1 :: String -> Data_br_6 -> (Set String, Locations) -> Err ((Set String, Locations), Data_br_1)
  naming_data_br_1 a b (i, d) =
    case b of
      Algebraic_data_6 e ->
        (
          (\(c, f) -> ((Prelude.foldl (\g -> \(Form_1 h _) -> Data.Set.insert h g) i f, c), Algebraic_data_1 f)) <$>
          naming_forms a e d)
      Branching_data_6 c e f -> second (Branching_data_1 c e) <$> naming_data_cases_1 a f (i, d)
      Struct_data_6 e f ->
        naming_name a e d >>= \(g, h) -> bimap ((, ) (Data.Set.insert h i)) (Struct_data_1 h) <$> naming_fields a f g
  naming_data_br_2 :: String -> Data_br_1 -> Locations -> Err Data_br_2
  naming_data_br_2 a b c =
    case b of
      Algebraic_data_1 d -> Right (Algebraic_data_2 d)
      Branching_data_1 f d e -> Branching_data_2 f d <$> naming_data_cases_2 a e c
      Struct_data_1 d e -> Right (Struct_data_2 d e)
  naming_data_case_1 :: String -> Data_case_6 -> (Set String, Locations) -> Err ((Set String, Locations), Data_case_1)
  naming_data_case_1 a (Data_case_6 b c d) e = second (Data_case_1 b c) <$> naming_data_br_1 a d e
  naming_data_case_2 :: String -> Data_case_1 -> Locations -> Err Data_case_2
  naming_data_case_2 a (Data_case_1 b c d) e = naming_names'' a c e >>= \(f, g) -> Data_case_2 b f <$> naming_data_br_2 a d g
  naming_data_cases_1 :: String -> [Data_case_6] -> (Set String, Locations) -> Err ((Set String, Locations), [Data_case_1])
  naming_data_cases_1 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_data_case_1 a d c >>= \(f, g) -> second ((:) g) <$> naming_data_cases_1 a e f
  naming_data_cases_2 :: String -> [Data_case_1] -> Locations -> Err [Data_case_2]
  naming_data_cases_2 a b c =
    case b of
      [] -> Right []
      d : e -> (:) <$> naming_data_case_2 a d c <*> naming_data_cases_2 a e c
  naming_datas_1 :: String -> [Data_6] -> (Set String, Locations) -> Err ((Set String, Locations), [Data_1])
  naming_datas_1 = naming_list naming_data_1
  naming_datas_2 :: String -> [Data_1] -> Locations -> Err [Data_2]
  naming_datas_2 f a b =
    case a of
      [] -> Right []
      c : d -> naming_data_2 f c b >>= \e -> (:) e <$> naming_datas_2 f d b
  naming_def_1 :: String -> Def_1 -> (Locations, Map' (Map' Location')) -> Err (Def_2, (Locations, Map' (Map' Location')))
  naming_def_1 i a (g, k) =
    case a of
      Basic_def_1 c @ (Name h j) b x d e -> (\(f, _) -> (Basic_def_2 h j b x d e, (f, k))) <$> naming_name i c g
      Instance_1 b (Name c l) d (Name f m, n, o) h e ->
        case Data.Map.lookup l k of
          Nothing ->
            Right
              (
                Instance_2 b (Name c l) d (Name f m, n, o) h e,
                (g, Data.Map.insert l (Data.Map.singleton m (Library (Location_1 i b))) k))
          Just p ->
            case Data.Map.lookup m p of
              Nothing ->
                Right
                  (
                    Instance_2 b (Name c l) d (Name f m, n, o) h e,
                    (g, Data.Map.adjust (Data.Map.insert m (Library (Location_1 i b))) l k))
              Just q -> Left (location_err ("instances of " ++ l ++ "{" ++ m ++ "}") q (Location_1 i b))
  naming_def_2 :: String -> Def_2 -> (Set String, Locations) -> Err Def_3
  naming_def_2 j a (m, b) =
    case a of
      Basic_def_2 k c d t f g -> naming_kt j d b >>= \(h, i) -> Basic_def_3 k c i t f <$> naming_expression j g (m, h)
      Instance_2 f c n (d, l, g) k e ->
        naming_patterns j g b >>= \(h, i) -> Instance_3 f c n (d, l, i) k <$> naming_nameexprs j (m, h) e
  naming_defs_1 :: String -> [Def_1] -> (Locations, Map' (Map' Location')) -> Err ([Def_2], (Locations, Map' (Map' Location')))
  naming_defs_1 a b c =
    case b of
      [] -> Right ([], c)
      d : e -> naming_def_1 a d c >>= \(f, g) -> first ((:) f) <$> naming_defs_1 a e g
  naming_defs_2 :: String -> [Def_2] -> (Set String, Locations) -> Err [Def_3]
  naming_defs_2 = naming_list' naming_def_2
  naming_expression :: String -> Expression_9 -> (Set String, Locations) -> Err Expression_1
  naming_expression g a (f, b) =
    case a of
      Application_expression_9 c d -> naming_application g (f, b) (naming_expression g c (f, b)) d
      Char_expression_9 c -> Right (Char_expression_1 c)
      Function_expression_9 c d -> naming_fun g (f, b) c d
      Int_expression_9 c -> Right (Int_expression_1 c)
      Let_expression_9 (Eqq' (Name l c) d e) h ->
        let
          i j = naming_application g (f, b) (naming_fun g (f, b) (Pat l j) h)
        in
          case Data.Set.member c f of
            False -> i (Name_pat c) (Prelude.foldr Function_expression_9 e d)
            True -> i (Application_pat c d) e
      Match_expression_9 h c d ->
        naming_expression g c (f, b) >>= \e -> Match_expression_1 h e <$> traverse (naming_case g (f, b)) d
      Name_expression_9 c d e -> Right (Name_expression_1 c d e)
  naming_fields :: String -> [(Name, Type_8)] -> Locations -> Err (Locations, [(String, Type_8)])
  naming_fields = naming_arguments naming_name
  naming_form :: String -> Form_6 -> Locations -> Err (Locations, Form_1)
  naming_form d (Form_6 a b) c = second (flip Form_1 b) <$> naming_name d a c
  naming_forms :: String -> [Form_6] -> Locations -> Err (Locations, [Form_1])
  naming_forms = naming_list naming_form
  naming_fun :: String -> (Set String, Locations) -> Pat -> Expression_9 -> Err Expression_1
  naming_fun x (d, b) z w = naming_pat x z (d, b) >>= \(a, c) -> Function_expression_1 c <$> naming_expression x w (d, a)
  naming_kt :: String -> KT0 -> Locations -> Err (Locations, KT1)
  naming_kt a (KT0 b c) d = naming_names'' a b d >>= \(e, f) -> second (KT1 e) <$> naming_arguments naming_name a c f
  naming_list :: (String -> t -> u -> Err (u, v)) -> String -> [t] -> u -> Err (u, [v])
  naming_list a h b c =
    case b of
      [] -> Right (c, [])
      d : e -> a h d c >>= \(f, g) -> second ((:) g) <$> naming_list a h e f
  naming_list' :: (String -> t -> u -> Err v) -> String -> [t] -> u -> Err [v]
  naming_list' a g b c =
    case b of
      [] -> Right []
      d : e -> a g d c >>= \f -> (:) f <$> naming_list' a g e c
  naming_method_0 :: String -> Method_9 -> Locations -> Err (Locations, Method_1)
  naming_method_0 a (Method_9 b c g d) e = second (\f -> Method_1 f c g d) <$> naming_name a b e
  naming_method_1 :: String -> Method_1 -> Locations -> Err Method_2
  naming_method_1 a (Method_1 b c g d) e = (\f -> Method_2 b f g d) <$> naming_args a c e
  naming_methods_0 :: String -> [Method_9] -> Locations -> Err (Locations, [Method_1])
  naming_methods_0 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_method_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_methods_0 a e f
  naming_methods_1 :: String -> [Method_1] -> Locations -> Err [Method_2]
  naming_methods_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_method_1 a d c >>= \g -> (:) g <$> naming_methods_1 a e c
  naming_name :: String -> Name -> Locations -> Err (Locations, String)
  naming_name f (Name a c) d =
    bimap (flip (location_err ("definitions of " ++ c)) (Location_1 f a)) (flip (,) c) (add d c (Library (Location_1 f a)))
  naming_nameexprs :: String -> (Set String, Locations) -> [(Name, Expression_9)] -> Err [(Name, Expression_1)]
  naming_nameexprs a b c =
    case c of
      [] -> Right []
      (d, e) : f -> naming_expression a e b >>= \g -> (:) (d, g) <$> naming_nameexprs a b f
  naming_names :: String -> [(Name, t)] -> Locations -> Err (Locations, [(String, t)])
  naming_names a b c =
    case b of
      [] -> Right (c, [])
      (d, e) : f -> naming_name a d c >>= \(g, h) -> second ((:) (h, e)) <$> naming_names a f g
  naming_names'' :: String -> [Name] -> Locations -> Err ([String], Locations)
  naming_names'' a b c =
    case b of
      [] -> Right ([], c)
      d : e -> naming_name a d c >>= \(f, g) -> first ((:) g) <$> naming_names'' a e f
  naming_ops :: String -> Locations -> [Opdecl_1] -> Err (Locations, [Name])
  naming_ops a b c =
    case c of
      [] -> Right (b, [])
      Opdecl_1 d e f : i ->
        let
          h = Location_1 a d
        in
          case Data.Map.lookup e b of
            Just g -> Left (location_err ("definitions of " ++ e) g h)
            Nothing -> second ((:) f) <$> naming_ops a (Data.Map.insert e (Library h) b) i
  naming_pat :: String -> Pat -> (Set String, Locations) -> Err (Locations, Pat)
  naming_pat a (Pat b c) (f, d) =
    case c of
      Application_pat g e -> second (\h -> Pat b (Application_pat g h)) <$> naming_pats a e (f, d)
      Blank_pat -> Right (d, Pat b Blank_pat)
      Name_pat e ->
        case Data.Set.member e f of
          False -> (\(g, _) -> (g, Pat b c)) <$> naming_name a (Name b e) d
          True -> Right (d, Pat b (Application_pat e []))
  naming_pats :: String -> [Pat] -> (Set String, Locations) -> Err (Locations, [Pat])
  naming_pats a b (f, c) =
    case b of
      [] -> Right (c, [])
      d : e -> naming_pat a d (f, c) >>= \(g, h) -> second ((:) h) <$> naming_pats a e (f, g)
  naming_pattern :: String -> Pattern_1 -> Locations -> Err (Locations, Pattern_0)
  naming_pattern f (Pattern_1 a c) d =
    case c of
      Blank_pattern -> Right (d, Blank_pattern)
      Name_pattern e -> second Name_pattern <$> naming_name f (Name a e) d
  naming_patterns :: String -> [Pattern_1] -> Locations -> Err (Locations, [Pattern_0])
  naming_patterns = naming_list naming_pattern
-----------------------------------------------------------------------------------------------------------------------------