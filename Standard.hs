--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Bifunctor
  import Data.Map
  import Tokenise
  import Tree
  data Alg_pat_7 =
    Application_alg_pat_7 Name [Alg_pat_7] | Blank_alg_pat_7 | Char_alg_pat_7 Char | Int_alg_pat_7 Integer | Name_alg_pat_7 Name
      deriving Show
  data Cat_1 =
    Cat_1 Location_0 (Name, [Name]) [Name] ((Location_0, Patn), (Location_0, Patn), Data_br_6, Expression_9, Expression_9)
      deriving Show
  data Class_7 = Class_7 Name [Name] [Name] (Name, Kind_0) (Maybe (Name, [Kind_0])) [Method_9] deriving Show
  data Def_1 =
    Basic_def_1 Name KT0 [Constraint_0] Type_8 Expression_9 |
    Instance_1
      Location_0
      [Name]
      [Name]
      Name
      [Kind_0]
      (Name, Maybe Kind_0, [Kind_0], [Pattern_1])
      [Constraint_0]
      [(Name, Expression_9)]
        deriving Show
  data Data_6 = Data_6 Name [(Name, Kind_0)] Data_br_6 deriving Show
  data Data_br_6 =
    Algebraic_data_6 [Form_6] | Branching_data_6 Location_0 Name [Data_case_6] | Struct_data_6 Name [(Name, Type_8)]
      deriving Show
  data Data_case_6 = Data_case_6 Name [Name] Data_br_6 deriving Show
  data Expression_9 =
    Application_expression_9 Expression_9 Expression_9 |
    Char_expression_9 Char |
    Function_expression_9 Pat_2 Expression_9 |
    Int_expression_9 Integer |
    Let_expression_9 Pat_2 Expression_9 Expression_9 |
    Match_expression_9 Location_0 Expression_9 [(Alg_pat_7, Expression_9)] |
    Name_expression_9 Name
      deriving Show
  data Form_6 = Form_6 Name [Type_8] deriving Show
  data Location' = Language | Library Location_1 deriving Show
  type Map' t = Map String t
  data Method_9 = Method_9 Name [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Op' = Op' Location_0 Op deriving Show
  data Pat_2 = Application_pat_2 Name [Pat_2] | Blank_pat_2 | Name_pat_2 Name deriving Show
  data Status = New | Old deriving (Eq, Show)
  data Tree_2 = Tree_2 [Data_6] [Cat_1] [Class_7] [Name] [Def_1] deriving Show
  data Tree_3 = Tree_3 [Name] Tree_2 deriving Show
  data Type_5 = Application_type_5 Type_5 Type_5 | Name_type_5 Name deriving Show
  data Type_8 = Type_8 Location_0 Type_5 deriving Show
  check_cat_m :: (Location_0 -> Location_1) -> Map' Op -> String -> (Pat, Expression_0) -> Err Expression_9
  check_cat_m f g a b =
    (
      std_inst f g b >>=
      \(Name e c, d) ->
        case a == c of
          False -> Left ("Expected method " ++ a ++ location (f e) ++ ", found " ++ c ++ "instead.")
          True -> Right d)
  gather_ops :: (Location_0 -> Location_1) -> Map' (Op, Status) -> [Opdecl_0] -> (Map' (Op, Status), [Name])
  gather_ops a b c =
    case c of
      [] -> (b, [])
      Opdecl_0 d e g h i : j -> second ((:) (Name d e)) (gather_ops a (ins_new e (Op h i g) b) j)
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  old :: Map' t -> Map' (t, Status)
  old = (<$>) (flip (,) Old)
  pop :: (Name -> t -> t -> t) -> [(Op', t)] -> t -> Op' -> [(Op', t)]
  pop f x expr (Op' l (Op pr assoc name)) =
    let
      u = (Op' l (Op pr assoc name), expr) : x
    in
      case x of
        [] -> u
        (Op' l' (Op pr' assoc' name'), expr') : x' ->
          case pr' < pr || pr' == pr && assoc' == Lft of
            False -> u
            True -> pop' pop f x' l' name' expr' expr (Op' l (Op pr assoc name))
  pop' ::
    (
      ((Name -> t -> t -> t) -> [(Op', t)] -> t -> u) ->
      (Name -> t -> t -> t) ->
      [(Op', t)] ->
      Location_0 ->
      String ->
      t ->
      t ->
      u)
  pop' h f x' l name expr' expr = h f x' (f (Name l name) expr' expr)
  pop_all :: (Name -> t -> t -> t) -> [(Op', t)] -> t -> t
  pop_all f x expr =
    case x of
      [] -> expr
      (Op' l (Op _ _ name), expr') : x' -> pop' pop_all f x' l name expr' expr
  rem_old :: Map' (t, Status) -> Map' t
  rem_old a = fst <$> Data.Map.filter (\(_, b) -> b == New) a
  shunting_yard ::
    (
      (Location_0 -> Location_1) ->
      String ->
      (t -> Err u, Name -> u -> u -> u) ->
      Map' Op ->
      [(Op', u)] ->
      t ->
      [(Name, t)] ->
      Err u)
  shunting_yard a k (f, g) ops x expr y =
    (
      f expr >>=
      \expr'' ->
        case y of
          [] -> Right (pop_all g x expr'')
          (Name l op, expr') : y' ->
            und_err op ops k (a l) (\op' -> shunting_yard a k (f, g) ops (pop g x expr'' (Op' l op')) expr' y'))
  standard_1 :: (Location_0 -> Location_1) -> Map' Op -> Tree_0 -> Err (Map' Op, Tree_2)
  standard_1 d f (Tree_0 a l b e c) =
      let
        (i, j) = gather_ops d (old f) e
      in
        (
          (\m -> \g -> \h -> \k -> (rem_old i, Tree_2 m g h j k)) <$>
          traverse (std_dat d) a <*>
          traverse (std_cat d (fst <$> i)) l <*>
          traverse (std_cls d) b <*>
          standard_defs d (fst <$> i) c)
  standard_arguments ::
    (Location_0 -> Location_1) -> Map' Op -> [(Pat, Type_7)] -> Type_7 -> Expression_0 -> Err (Type_8, Expression_9)
  standard_arguments a m b c d =
    case b of
      [] -> (,) <$> std_type a c <*> std_expr a m d
      (e, Type_7 l f) : g ->
        (
          (\q -> \h -> \(Type_8 i j, k) ->
            (
              Type_8 i (Application_type_5 (Application_type_5 (Name_type_5 (Name l "Arrow")) h) j),
              Function_expression_9 q k)) <$>
          std_pat a m e <*>
          std_type' a f <*>
          standard_arguments a m g c d)
  standard_def :: (Location_0 -> Location_1) -> Map' Op -> Def_0 -> Err Def_1
  standard_def i j a =
    case a of
      Basic_def_0 b c g d e f -> uncurry (Basic_def_1 b c g) <$> standard_arguments i j d e f
      Instance_def_0 b c d f g h k e -> Instance_1 b c d f g h k <$> traverse (std_inst i j) e
  standard_defs :: (Location_0 -> Location_1) -> Map' Op -> [Def_0] -> Err [Def_1]
  standard_defs a b = traverse (standard_def a b)
  std_apat :: (Location_0 -> Location_1) -> Map' Op -> Alg_pat -> Err Alg_pat_7
  std_apat a b c =
    case c of
      Application_alg_pat d f -> Application_alg_pat_7 d <$> traverse (std_apat a b) f
      Blank_alg_pat -> Right Blank_alg_pat_7
      Char_alg_pat d -> Right (Char_alg_pat_7 d)
      Int_alg_pat d -> Right (Int_alg_pat_7 d)
      Name_alg_pat d -> Right (Name_alg_pat_7 d)
      Op_alg_pat d e -> shunting_yard a "operator" (std_apat a b, \f -> \g -> \h -> Application_alg_pat_7 f [g, h]) b [] d e
  std_cat :: (Location_0 -> Location_1) -> Map' Op -> Cat_0 -> Err Cat_1
  std_cat a b (Cat_0 c d q (e, f, g, h, i)) =
    (
      (\l -> \m -> \n -> Cat_1 c d q (e, f, l, m, n)) <$>
      std_dat_br a g <*>
      check_cat_m a b "Compose" h <*>
      check_cat_m a b "Id" i)
  std_cls :: (Location_0 -> Location_1) -> Class_0 -> Err Class_7
  std_cls e (Class_0 a b c f g d) = Class_7 a b c f g <$> traverse (std_mthd e) d
  std_dat :: (Location_0 -> Location_1) -> Data_0 -> Err Data_6
  std_dat a (Data_0 b d e) = Data_6 b d <$> std_dat_br a e
  std_dat_br :: (Location_0 -> Location_1) -> Data_br_0 -> Err Data_br_6
  std_dat_br a b =
    case b of
      Algebraic_data_0 c -> Algebraic_data_6 <$> traverse (\(Form_0 d e) -> Form_6 d <$> traverse (std_type a) e) c
      Branching_data_0 h c d -> Branching_data_6 h c <$> traverse (\(Data_case_0 e f g) -> Data_case_6 e f <$> std_dat_br a g) d
      Struct_data_0 c d -> Struct_data_6 c <$> traverse (\(e, f) -> (,) e <$> std_type a f) d
  std_expr :: (Location_0 -> Location_1) -> Map' Op -> Expression_0 -> Err Expression_9
  std_expr a f b =
    case b of
      Application_expression_0 c d -> Prelude.foldl Application_expression_9 <$> std_expr a f c <*> traverse (std_expr a f) d
      Char_expression_0 c -> Right (Char_expression_9 c)
      Function_expression_0 c d -> Function_expression_9 <$> std_pat a f c <*> std_expr a f d
      Int_expression_0 c -> Right (Int_expression_9 c)
      Let_expression_0 (c, e) d ->
        (
          (\(g, h) -> \i -> Let_expression_9 g (Prelude.foldr Function_expression_9 i h)) <$>
          std_pat'' a f c <*>
          std_expr a f e <*>
          std_expr a f d)
      Match_expression_0 c d e ->
        Match_expression_9 c <$> std_expr a f d <*> traverse (\(g, h) -> (,) <$> std_apat a f g <*> std_expr a f h) e
      Name_expression_0 c -> Right (Name_expression_9 c)
      Op_expression_0 c d ->
        shunting_yard
          a
          "operator"
          (std_expr a f, \e -> \g -> Application_expression_9 (Application_expression_9 (Name_expression_9 e) g))
          f
          []
          c
          d
  std_inst :: (Location_0 -> Location_1) -> Map' Op -> (Pat, Expression_0) -> Err (Name, Expression_9)
  std_inst a b (c, d) = (\(e, f) -> \g -> (e, Prelude.foldr Function_expression_9 g f)) <$> std_pat' a b c <*> std_expr a b d
  std_mthd :: (Location_0 -> Location_1) -> Method -> Err Method_9
  std_mthd a (Method b c d e) = Method_9 b c d <$> std_type a e
  std_pat :: (Location_0 -> Location_1) -> Map' Op -> Pat -> Err Pat_2
  std_pat a b c =
    case c of
      Application_pat d e ->
        case d of
          Blank_pat f -> Left ("Invalid blank pattern" ++ location' (a f))
          Name_pat (Name f g) -> Left ("Invalid name pattern " ++ g ++ location' (a f))
          _ -> (\(Application_pat_2 f g) -> \h -> Application_pat_2 f (g ++ h)) <$> std_pat a b d <*> traverse (std_pat a b) e
      Blank_pat _ -> Right Blank_pat_2
      Constr_pat d -> Right (Application_pat_2 d [])
      Name_pat d -> Right (Name_pat_2 d)
      Op_pat d e -> shunting_yard a "operator" (std_pat a b, \f -> \g -> \h -> Application_pat_2 f [g, h]) b [] d e
  std_pat' :: (Location_0 -> Location_1) -> Map' Op -> Pat -> Err (Name, [Pat_2])
  std_pat' a b c =
    case c of
      Application_pat d e -> (\(f, g) -> \h -> (f, g ++ h)) <$> std_pat' a b d <*> traverse (std_pat a b) e
      Blank_pat d -> Left ("Invalid blank pattern" ++ location' (a d))
      Constr_pat d -> Right (d, [])
      Name_pat d -> Right (d, [])
      Op_pat d e ->
        (
          (\(Application_pat_2 f [g, h]) -> (f, [g, h])) <$>
          shunting_yard a "operator" (std_pat a b, \f -> \g -> \h -> Application_pat_2 f [g, h]) b [] d e)
  std_pat'' :: (Location_0 -> Location_1) -> Map' Op -> Pat -> Err (Pat_2, [Pat_2])
  std_pat'' a b c =
    case c of
      Application_pat d e ->
        (
          (\(f, g) -> \h ->
            case f of
              Application_pat_2 i j -> (Application_pat_2 i (j ++ h), [])
              _ -> (f, g ++ h)) <$>
          std_pat'' a b d <*>
          traverse (std_pat a b) e)
      Blank_pat _ -> Right (Blank_pat_2, [])
      Constr_pat d -> Right (Application_pat_2 d [], [])
      Name_pat d -> Right (Name_pat_2 d, [])
      Op_pat d e ->
        (\f -> (f, [])) <$> shunting_yard a "operator" (std_pat a b, \f -> \g -> \h -> Application_pat_2 f [g, h]) b [] d e
  std_type :: (Location_0 -> Location_1) -> Type_7 -> Err Type_8
  std_type c (Type_7 a b) = Type_8 a <$> std_type' c b
  std_type' :: (Location_0 -> Location_1) -> Type_0 -> Err Type_5
  std_type' e b =
    case b of
      Application_type_0 c d -> Prelude.foldl Application_type_5 <$> std_type' e c <*> traverse (std_type' e) d
      Name_type_0 a -> Right (Name_type_5 a)
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
--------------------------------------------------------------------------------------------------------------------------------