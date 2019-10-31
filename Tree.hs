--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tree (
  Alg_pat (..),
  Assoc (..),
  Cat_0 (..),
  Class_0 (..),
  Data_0 (..),
  Data_br_0 (..),
  Data_case_0 (..),
  Def_0 (..),
  Expression_0 (..),
  Form_0 (..),
  Kind_0 (..),
  Method (..),
  Name (..),
  Op (..),
  Opdecl_0 (..),
  Pat (..),
  Patn (..),
  Pattern_1 (..),
  Tree_0 (..),
  Tree_1 (..),
  Type_0 (..),
  Type_7 (..),
  init_location,
  parse_expression,
  parse_tree) where
  import Control.Applicative
  import Control.Monad.Trans.State.Strict
  import Data.Char
  import Data.Bifunctor
  import Tokenise
  data Alg_pat =
    Application_alg_pat Name [Alg_pat] |
    Blank_alg_pat |
    Int_alg_pat Integer |
    Name_alg_pat Name |
    Op_alg_pat Alg_pat [(Name, Alg_pat)]
      deriving Show
  data Assoc = Lft | Rght deriving (Eq, Show)
  data Cat_0 =
    Cat_0
      Location_0
      (Name, [Name])
      [Name]
      ((Location_0, Patn), (Location_0, Patn), Data_br_0, (Pat, Expression_0), (Pat, Expression_0))
        deriving Show
  data Class_0 = Class_0 Name [Name] [Name] (Name, Kind_0) [Method] deriving Show
  data Data_0 = Data_0 Name [(Name, Kind_0)] Data_br_0 deriving Show
  data Data_br_0 =
    Algebraic_data_0 [Form_0] | Branching_data_0 Location_0 Name [Data_case_0] | Struct_data_0 Name [(Name, Type_7)]
      deriving Show
  data Data_case_0 = Data_case_0 Name [Name] Data_br_0 deriving Show
  data Def_0 =
    Basic_def_0 Name [Name] [Name] [(Name, Kind_0)] [(Pat, Type_7)] Type_7 Expression_0 |
    Instance_def_0 Location_0 [Name] [Name] Name [Kind_0] (Name, Maybe Kind_0, [Kind_0], [Pattern_1]) [(Pat, Expression_0)]
      deriving Show
  data Expression_0 =
    Application_expression_0 Expression_0 [Expression_0] |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Let_expression_0 (Pat, Expression_0) Expression_0 |
    Match_expression_0 Location_0 Expression_0 [(Alg_pat, Expression_0)] |
    Name_expression_0 Name |
    Op_expression_0 Expression_0 [(Name, Expression_0)]
      deriving Show
  data Form_0 = Form_0 Name [Type_7] deriving Show
  data Kind_0 =
    Application_kind_0 Kind_0 [Kind_0] | Arrow_kind_0 Kind_0 Kind_0 | Intersection_kind_0 [Kind_0] | Name_kind_0 Name
      deriving Show
  data Method = Method Name [(Name, Kind_0)] Type_7 deriving Show
  data Name = Name Location_0 String deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Opdecl_0 = Opdecl_0 Location_0 String String Integer Assoc deriving Show
  data Pat = Application_pat Pat [Pat] | Blank_pat Location_0 | Constr_pat Name | Name_pat Name | Op_pat Pat [(Name, Pat)]
    deriving Show
  data Patn = Application_patn Name [Patn] | Name_patn Name deriving Show
  data Pattern_1 = Blank_pattern_1 | Name_pattern_1 Name deriving Show
  type Parser = StateT State' (Either Location_0)
  data State' = State' Tokens Location_0 deriving Show
  data Tree_0 = Tree_0 [Opdecl_0] [Data_0] [Cat_0] [Class_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Application_type_0 Type_0 [Type_0] | Name_type_0 Name deriving Show
  data Type_7 = Type_7 Location_0 Type_0 deriving Show
  infixl 3 <+>
  (<+>) :: Parser t -> Parser t -> Parser t
  StateT a <+> StateT b =
    StateT
      (\c ->
        case (a c, b c) of
          (Left d, Left e) -> Left (max d e)
          (Left d, Right (e, f)) -> Right (e, update_location f d)
          (Right (d, e), Left f) -> Right (d, update_location e f)
          (Right (d, State' e f), Right (g, State' h i)) ->
            Right
              (case compare (current_line_and_char e) (current_line_and_char h) of
                LT -> (g, State' h i)
                _ -> (d, State' e f)))
  empty_parser :: Parser t
  empty_parser = StateT (\(State' _ l) -> Left l)
  filter_cap :: Parser Name -> Parser Name
  filter_cap = filter_parser (\(Name _ (a : _)) -> isUpper a)
  filter_low :: Parser Name -> Parser Name
  filter_low = filter_parser (\(Name _ (a : _)) -> isLower a)
  filter_parser :: (t -> Bool) -> Parser t -> Parser t
  filter_parser f p = p >>= \x -> if f x then return x else empty_parser
  init_location :: Location_0
  init_location = Location_0 0 0
  int_to_nat_type_0 :: Location_0 -> Integer -> Type_0
  int_to_nat_type_0 l x =
    case x of
      0 -> Name_type_0 (Name l "Zero")
      _ -> Application_type_0 (Name_type_0 (Name l "Next")) [int_to_nat_type_0 l (x - 1)]
  parse :: Parser t -> (Location_0 -> Location_1) -> String -> Err t
  parse a b c =
    let
       d = parse_error b
    in
      (
        tokenise b c >>=
        \e ->
          case runStateT a (State' e init_location) of
            Left f -> d f
            Right (f, State' (Tokens h _) g) ->
              case h of
                [] -> Right f
                _ -> d g)
  parse_alg_pattern :: Parser Alg_pat
  parse_alg_pattern = parse_op_alg_pattern <+> parse_application_alg_pattern <+> parse_elementary_alg_pattern
  parse_algebraic :: Parser Data_br_0
  parse_algebraic = Algebraic_data_0 <$ parse_token Algebraic_token <*> parse_optional parse_curlies parse_form
  parse_ap_expr :: Parser Expression_0
  parse_ap_expr = Application_expression_0 <$> parse_br_expr <*> parse_some parse_br_expr
  parse_ap_pat :: Parser Pat
  parse_ap_pat = Application_pat <$> parse_br_pat <*> parse_some parse_br_pat
  parse_ap_patn :: Parser Patn
  parse_ap_patn = Application_patn <$> parse_cap <*> parse_some parse_br_patn
  parse_ap_type :: Parser Type_0
  parse_ap_type = Application_type_0 <$> parse_br_type <*> parse_some parse_br_type
  parse_application_alg_pattern :: Parser Alg_pat
  parse_application_alg_pattern = Application_alg_pat <$> parse_cap <*> parse_some parse_br_alg_pattern
  parse_application_kind :: Parser Kind_0
  parse_application_kind = Application_kind_0 <$> parse_atomic_kind <*> parse_some parse_atomic_kind
  parse_argument :: Parser t -> Parser u -> Parser (t, u)
  parse_argument p = (<*>) ((<*) ((,) <$> p) parse_colon)
  parse_arguments :: (Parser [(t, u)] -> Parser [(t, u)]) -> Parser t -> Parser u -> Parser [(t, u)]
  parse_arguments a b c = parse_optional a (parse_argument b c)
  parse_arguments' :: Parser t -> Parser [(t, Type_7)]
  parse_arguments' a = parse_arguments parse_round a parse_type
  parse_arrow :: Parser ()
  parse_arrow = parse_operator "->"
  parse_arrow' :: Parser (Expression_0 -> t) -> Parser t
  parse_arrow' p = p <* parse_arrow <*> parse_expression'
  parse_arrow_kind :: Parser Kind_0
  parse_arrow_kind = Arrow_kind_0 <$> parse_simple_kind <* parse_arrow <*> parse_kind
  parse_arrow_type :: Parser Type_0
  parse_arrow_type =
    (
      (\a -> \b -> \c -> Application_type_0 (Name_type_0 (Name b "Arrow")) [a, c]) <$>
      parse_br_type' <*>
      (return <$> parse_location <*> parse_arrow) <*>
      parse_type')
  parse_atomic_kind :: Parser Kind_0
  parse_atomic_kind = parse_round parse_kind <+> parse_intersection_kind <+> parse_name_kind
  parse_basic :: Parser Def_0
  parse_basic =
    (
      Basic_def_0 <$>
      filter_low (parse_name'' Def_token) <*>
      parse_kind_vars <*>
      parse_cat_constrs <*>
      parse_kinds <*>
      parse_optional parse_round (first (uncurry Op_pat) <$> parse_pat_and_type) <*
      parse_colon <*>
      parse_type <*
      parse_eq <*>
      parse_expression')
  parse_blank_alg_pattern :: Parser Alg_pat
  parse_blank_alg_pattern = Blank_alg_pat <$ parse_token Blank_token
  parse_blank_pat :: Parser Pat
  parse_blank_pat = Blank_pat <$> parse_location <* parse_token Blank_token
  parse_br_alg_pattern :: Parser Alg_pat
  parse_br_alg_pattern = parse_round (parse_op_alg_pattern <+> parse_application_alg_pattern) <+> parse_elementary_alg_pattern
  parse_br_alg_pattern' :: Parser Alg_pat
  parse_br_alg_pattern' = parse_application_alg_pattern <+> parse_round parse_op_alg_pattern <+> parse_elementary_alg_pattern
  parse_br_expr :: Parser Expression_0
  parse_br_expr = parse_round (parse_comp_expr <+> parse_ap_expr) <+> parse_elementary_expression
  parse_br_expr' :: Parser Expression_0
  parse_br_expr' = parse_round parse_comp_expr <+> parse_ap_expr <+> parse_elementary_expression
  parse_br_pat :: Parser Pat
  parse_br_pat = parse_round (parse_op_pat <+> parse_ap_pat) <+> parse_elementary_pat
  parse_br_pat' :: Parser Pat
  parse_br_pat' = parse_ap_pat <+> parse_round parse_op_pat <+> parse_elementary_pat
  parse_br_patn :: Parser Patn
  parse_br_patn = parse_round parse_ap_patn <+> parse_elementary_patn
  parse_br_type :: Parser Type_0
  parse_br_type = parse_round (parse_arrow_type <+> parse_ap_type) <+> parse_elementary_type
  parse_br_type' :: Parser Type_0
  parse_br_type' = parse_round parse_arrow_type <+> parse_ap_type <+> parse_elementary_type
  parse_brackets :: Token_0 -> Parser t -> Token_0 -> Parser t
  parse_brackets a b c = parse_token a *> b <* parse_token c
  parse_branch :: Parser Data_br_0
  parse_branch =
    (
      Branching_data_0 <$>
      parse_location <*
      parse_token Branch_token <*>
      parse_low <*>
      parse_optional parse_curlies parse_data_case)
  parse_cap :: Parser Name
  parse_cap = filter_cap parse_name'
  parse_cat :: Parser Cat_0
  parse_cat =
    (
      Cat_0 <$>
      parse_location <*
      parse_token Cat_token <*>
      parse_curlies (parse_curlies ((,) <$> parse_cap <*> parse_many parse_low)) <*>
      parse_cat_constrs <*>
      parse_round
        (
          (,,,,) <$>
          ((,) <$> parse_location <*> parse_patn) <*
          parse_arrow <*>
          ((,) <$> parse_location <*> parse_patn) <*
          parse_eq <*>
          parse_data_br <*
          parse_comma <*>
          parse_method_impl <*
          parse_comma <*>
          parse_method_impl))
  parse_cat_constr :: Parser Name
  parse_cat_constr = parse_token Cat_token *> parse_low
  parse_cat_constrs :: Parser [Name]
  parse_cat_constrs = parse_optional (\a -> parse_operator "<<" *> a <* parse_operator ">>") parse_cat_constr
  parse_class :: Parser Class_0
  parse_class =
    (
      Class_0 <$>
      filter_cap (parse_name'' Class_token) <*>
      parse_kind_vars <*>
      parse_cat_constrs <*>
      parse_curlies ((,) <$> parse_low <* parse_colon <*> parse_kind) <*>
      parse_optional parse_round parse_method)
  parse_colon :: Parser ()
  parse_colon = parse_operator "::"
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_comp_expr :: Parser Expression_0
  parse_comp_expr = parse_let_expression <+> parse_match_expression <+> parse_function <+> parse_op_expr
  parse_curlies :: Parser t -> Parser t
  parse_curlies a = parse_brackets Left_curly_token a Right_curly_token
  parse_data :: Parser Data_0
  parse_data = Data_0 <$> filter_cap (parse_name'' Data_token) <*> parse_kinds <* parse_eq <*> parse_data_br
  parse_data_br :: Parser Data_br_0
  parse_data_br = parse_algebraic <+> parse_branch <+> parse_struct
  parse_data_case :: Parser Data_case_0
  parse_data_case = Data_case_0 <$> parse_cap <*> parse_many parse_low <* parse_arrow <*> parse_data_br
  parse_def :: Parser Def_0
  parse_def = parse_basic <+> parse_instance
  parse_elementary :: (Token_0 -> Maybe t) -> Parser t
  parse_elementary a =
    StateT
      (\(State' tokens d) ->
        bimap (max d) (second (\tokens' -> State' tokens' (max d (current_line_and_char tokens')))) (nom_token a tokens))
  parse_elementary_alg_pattern :: Parser Alg_pat
  parse_elementary_alg_pattern =
    (
      parse_blank_alg_pattern <+>
      parse_int_alg_pattern <+>
      (\a -> Application_alg_pat a []) <$> parse_cap <+>
      Name_alg_pat <$> parse_low)
  parse_elementary_expression :: Parser Expression_0
  parse_elementary_expression = parse_int_expression <+> parse_name_expression
  parse_elementary_pat :: Parser Pat
  parse_elementary_pat = parse_blank_pat <+> Name_pat <$> parse_low <+> Constr_pat <$> parse_cap
  parse_elementary_patn :: Parser Patn
  parse_elementary_patn = (\a -> Application_patn a []) <$> parse_cap <+> Name_patn <$> parse_low
  parse_elementary_type :: Parser Type_0
  parse_elementary_type = parse_name_type <+> (int_to_nat_type_0 <$> parse_location <*> parse_int')
  parse_eq :: Parser ()
  parse_eq = parse_operator "="
  parse_error :: (Location_0 -> Location_1) -> Location_0 -> Err t
  parse_error a b = Left ("Parse error" ++ location' (a b))
  parse_expression :: String -> Err Expression_0
  parse_expression = parse parse_expression' (Location_1 "input")
  parse_expression' :: Parser Expression_0
  parse_expression' = parse_comp_expr <+> parse_ap_expr <+> parse_elementary_expression
  parse_form :: Parser Form_0
  parse_form = Form_0 <$> parse_cap <*> parse_many (Type_7 <$> parse_location <*> parse_br_type)
  parse_function :: Parser Expression_0
  parse_function = parse_arrow' (Function_expression_0 <$> parse_pat)
  parse_instance :: Parser Def_0
  parse_instance =
    (
      Instance_def_0 <$>
      parse_location <*
      parse_token Instance_token <*>
      parse_kind_vars <*>
      parse_cat_constrs <*>
      parse_cap <*>
      parse_kinds' <*>
      parse_curlies
        (
          (,,,) <$>
          parse_cap <*>
          parse_optional' (Just <$> parse_curlies (parse_curlies parse_kind)) <*>
          parse_optional (parse_sq <$> parse_sq) parse_kind <*>
          parse_many parse_pattern_1) <*>
      parse_optional parse_round parse_method_impl)
  parse_int :: Parser Integer
  parse_int = negate <$ parse_token Negate_token <*> parse_int' <+> parse_int'
  parse_int' :: Parser Integer
  parse_int' =
    parse_elementary
      (\a ->
        case a of
          Int_token b -> Just b
          _ -> Nothing)
  parse_int_alg_pattern :: Parser Alg_pat
  parse_int_alg_pattern = Int_alg_pat <$> parse_int
  parse_int_expression :: Parser Expression_0
  parse_int_expression = Int_expression_0 <$> parse_int
  parse_intersection_kind :: Parser Kind_0
  parse_intersection_kind = Intersection_kind_0 <$ parse_operator "<" <*> parse_list 1 parse_kind <* parse_operator ">"
  parse_let_expression :: Parser Expression_0
  parse_let_expression =
    (
      flip (Prelude.foldr Let_expression_0) <$
      parse_token Let_token <*>
      parse_list 1 ((,) <$> parse_pat <* parse_eq <*> parse_expression') <*
      parse_token In_token <*>
      parse_expression')
  parse_kind :: Parser Kind_0
  parse_kind = parse_arrow_kind <+> parse_application_kind <+> parse_atomic_kind
  parse_kind_vars :: Parser [Name]
  parse_kind_vars = parse_optional (parse_sq <$> parse_sq) parse_low
  parse_kinds :: Parser [(Name, Kind_0)]
  parse_kinds = parse_arguments parse_sq parse_low parse_kind
  parse_kinds' :: Parser [Kind_0]
  parse_kinds' = parse_optional (parse_sq <$> parse_sq) parse_kind
  parse_list :: Integer -> Parser t -> Parser [t]
  parse_list i p =
    case i of
      0 -> parse_list 1 p <+> return []
      1 -> (:) <$> p <*> parse_many (parse_comma *> p)
      _ -> (:) <$> p <* parse_comma <*> parse_list (i - 1) p
  parse_load :: Parser Name
  parse_load =
    filter_cap (parse_name_3 Load_token ((flip (++) ".awf" <$> parse_name) <* parse_operator "." <* parse_name_4 "awf"))
  parse_location :: Parser Location_0
  parse_location = (\(State' tokens _) -> current_line_and_char tokens) <$> get
  parse_low :: Parser Name
  parse_low = filter_low parse_name'
  parse_many :: Parser t -> Parser [t]
  parse_many a = parse_some a <+> return []
  parse_match_expression :: Parser Expression_0
  parse_match_expression =
    (
      Match_expression_0 <$>
      parse_location <*
      parse_token Match_token <*>
      parse_expression' <*
      parse_token Of_token <*>
      parse_optional parse_curlies (parse_arrow' ((,) <$> parse_alg_pattern)))
  parse_method :: Parser Method
  parse_method = Method <$> parse_low <*> parse_kinds <* parse_colon <*> parse_type
  parse_name :: Parser String
  parse_name =
    parse_elementary
      (\a ->
        case a of
          Name_token b -> Just b
          _ -> Nothing)
  parse_method_impl :: Parser (Pat, Expression_0)
  parse_method_impl = (,) <$> parse_pat <* parse_eq <*> parse_expression'
  parse_name' :: Parser Name
  parse_name' = Name <$> parse_location <*> parse_name
  parse_name'' :: Token_0 -> Parser Name
  parse_name'' = flip parse_name_3 parse_name
  parse_name_3 :: Token_0 -> Parser String -> Parser Name
  parse_name_3 a b = Name <$> parse_location <* parse_token a <*> b
  parse_name_4 :: String -> Parser ()
  parse_name_4 = parse_token <$> Name_token
  parse_name_expression :: Parser Expression_0
  parse_name_expression = Name_expression_0 <$> parse_name'
  parse_name_kind :: Parser Kind_0
  parse_name_kind = Name_kind_0 <$> parse_name'
  parse_name_type :: Parser Type_0
  parse_name_type = Name_type_0 <$> parse_name'
  parse_op :: Parser String
  parse_op =
    parse_elementary
      (\a ->
        case a of
          Operator_token b -> Just b
          _ -> Nothing)
  parse_op_0 :: Parser String
  parse_op_0 = filter_parser (\x -> notElem x ["->", "::", "="]) parse_op
  parse_op_0' :: Parser Name
  parse_op_0' = Name <$> parse_location <*> parse_op_0
  parse_op_alg_pattern :: Parser Alg_pat
  parse_op_alg_pattern = Op_alg_pat <$> parse_br_alg_pattern' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_alg_pattern')
  parse_op_expr :: Parser Expression_0
  parse_op_expr = Op_expression_0 <$> parse_br_expr' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_expr')
  parse_op_pat :: Parser Pat
  parse_op_pat = Op_pat <$> parse_br_pat' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_pat')
  parse_opdecl :: Parser Opdecl_0
  parse_opdecl =
    (
      (\a -> \b -> \c -> \d -> \e -> Opdecl_0 a d e c b) <$>
      parse_location <*>
      (Lft <$ parse_token Infixl_token <+> Rght <$ parse_token Infixr_token) <*>
      parse_int <*>
      parse_op_0 <*
      parse_eq <*>
      parse_name)
  parse_operator :: String -> Parser ()
  parse_operator x = parse_token (Operator_token x)
  parse_optional :: (Parser [t] -> Parser [t]) -> Parser t -> Parser [t]
  parse_optional a b = parse_optional' (a (parse_list 1 b))
  parse_optional' :: Alternative f => Parser (f t) -> Parser (f t)
  parse_optional' a = a <+> pure empty
  parse_pat :: Parser Pat
  parse_pat = parse_op_pat <+> parse_ap_pat <+> parse_elementary_pat
  parse_patn :: Parser Patn
  parse_patn = parse_ap_patn <+> parse_elementary_patn
  parse_pat_and_type :: Parser ((Pat, [(Name, Pat)]), Type_7)
  parse_pat_and_type =
    (
      (\a -> first ((,) a)) <$>
      parse_br_pat' <*>
      (
        (,) [] <$ parse_colon <*> parse_type <+>
        (
          (\a -> first (\(b, c) -> (a, b) : c)) <$>
          (Name <$> parse_location <*> parse_op_0) <*>
          parse_pat_and_type)))
  parse_pattern_1 :: Parser Pattern_1
  parse_pattern_1 = Blank_pattern_1 <$ parse_token Blank_token <+> Name_pattern_1 <$> parse_low
  parse_round :: Parser t -> Parser t
  parse_round a = parse_brackets Left_round_token a Right_round_token
  parse_simple_kind :: Parser Kind_0
  parse_simple_kind = parse_atomic_kind <+> parse_application_kind
  parse_some :: Parser t -> Parser [t]
  parse_some a = (:) <$> a <*> parse_many a
  parse_sq :: Parser t -> Parser t
  parse_sq a = parse_brackets Left_square_token a Right_square_token
  parse_struct :: Parser Data_br_0
  parse_struct = Struct_data_0 <$> filter_cap (parse_name'' Struct_token) <*> parse_arguments' parse_low
  parse_token :: Token_0 -> Parser ()
  parse_token a = parse_elementary (\b -> if b == a then Just () else Nothing)
  parse_tree :: (Location_0 -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser Tree_1
  parse_tree' =
    (
      Tree_1 <$>
      parse_many parse_load <*>
      (
        Tree_0 <$>
        parse_many parse_opdecl <*>
        parse_many parse_data <*>
        parse_many parse_cat <*>
        parse_many parse_class <*>
        parse_many parse_def))
  parse_type :: Parser Type_7
  parse_type = Type_7 <$> parse_location <*> parse_type'
  parse_type' :: Parser Type_0
  parse_type' = parse_arrow_type <+> parse_ap_type <+> parse_elementary_type
  update_location :: State' -> Location_0 -> State'
  update_location (State' a b) c = State' a (max b c)
--------------------------------------------------------------------------------------------------------------------------------