--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tree where
  import Control.Applicative
  import Control.Monad
  import Data.Bifunctor
  import Data.Char
  import Tokenise
  data Alg_pat =
    Application_alg_pat Name [Alg_pat] |
    Blank_alg_pat |
    Char_alg_pat Char |
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
      ((Location_0, Pat), (Location_0, Pat), Data_br_0, [Pat], Expression_0, [Pat], Expression_0)
        deriving Show
  data Class_0 = Class_0 Name [Name] [Name] (Name, Kind_0) (Maybe (Name, [Kind_0])) [Method] deriving Show
  data Constraint_0 = Constraint_0 Name [Kind_0] Name deriving Show
  data Data_0 = Data_0 Location_0 String [(Name, Kind_0)] Data_br_0 deriving Show
  data Data_br_0 =
    Algebraic_data_0 [Form_0] | Branching_data_0 Location_0 Name [Data_case_0] | Struct_data_0 Name [(Name, Type_7)]
      deriving Show
  data Data_case_0 = Data_case_0 Name [Name] Data_br_0 deriving Show
  data Def_0 =
    Basic_def_0 Name KT0 [Constraint_0] [(Pat, Type_7)] Type_7 Expression_0 |
    Instance_def_0
      Location_0
      [Name]
      [Name]
      Name
      [Kind_0]
      (Name, Maybe Kind_0, [Kind_0], [Pattern_1])
      [Constraint_0]
      [(Pat, Expression_0)]
        deriving Show
  data Expression_0 =
    Application_expression_0 Expression_0 [Expression_0] |
    Char_expression_0 Char |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Let_expression_0 (Pat, Expression_0) Expression_0 |
    Match_expression_0 Location_0 Expression_0 [(Alg_pat, Expression_0)] |
    -- Name_expression_0 Name (Maybe Type_7) [Type_7] |
    Name_expression_0 Name |
    Op_expression_0 Expression_0 [(Name, Expression_0)]
      deriving Show
  data Form_0 = Form_0 Name [Type_7] deriving Show
  data Kind_0 = Application_kind_0 Kind_0 Kind_0 | Name_kind_0 Name deriving Show
  data KT0 = KT0 [Name] [Name] [(Name, Kind_0)] deriving Show
  data Method = Method Name [(Name, Kind_0)] [Constraint_0] Type_7 deriving Show
  data Name = Name Location_0 String deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Opdecl_0 = Opdecl_0 Location_0 String String Integer Assoc deriving Show
  data Pat = Application_pat Pat [Pat] | Blank_pat Location_0 | Constr_pat Name | Name_pat Name | Op_pat Pat [(Name, Pat)]
    deriving Show
  data Pattern_1 = Blank_pattern_1 | Name_pattern_1 Name deriving Show
  newtype Parser s f t = Parser {parser :: s -> f (t, s)}
  type Parser' = Parser State (Either Location_0)
  data State = State Tokens Location_0 deriving Show
  data Tree_0 = Tree_0 [Data_0] [Cat_0] [Class_0] [Opdecl_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Application_type_0 Type_0 [Type_0] | Name_type_0 Name (Maybe Kind_0) [Kind_0] deriving Show
  data Type_7 = Type_7 Location_0 Type_0 deriving Show
  infixl 4 <&
  (<&) :: (Location_0 -> t) -> Parser' () -> Parser' t
  f <& p = f <$> parse_location <* p
  infixl 4 <&>
  (<&>) :: (Location_0 -> t -> u) -> Parser' t -> Parser' u
  f <&> p = f <$> parse_location <*> p
  infixl 3 <+>
  (<+>) :: Parser' t -> Parser' t -> Parser' t
  Parser a <+> Parser b = Parser (\c -> left_bind (\d -> b (update_location c d)) (a c))
  instance Monad f => Applicative (Parser s f) where
    Parser a <*> Parser b = Parser (a >=> \(c, d) -> first c <$> b d)
    pure x = Parser (\y -> return (x, y))
  instance Functor f => Functor (Parser s f) where
    fmap a (Parser b) = Parser (\c -> first a <$> b c)
  instance Monad f => Monad (Parser s f) where
    Parser p >>= f = Parser (p >=> \(y, z) -> parser (f y) z)
  empty_parser :: Parser' t
  empty_parser = Parser (\(State _ l) -> Left l)
  filter_parser :: (t -> Bool) -> Parser' t -> Parser' t
  filter_parser f p = p >>= \x -> if f x then return x else empty_parser
  init_location :: Location_0
  init_location = Location_0 0 0
  int_to_nat_type_0 :: Location_0 -> Integer -> Type_0
  int_to_nat_type_0 l x =
    case x of
      0 -> Name_type_0 (Name l "Zero") Nothing []
      _ -> Application_type_0 (Name_type_0 (Name l "Next") Nothing []) [int_to_nat_type_0 l (x - 1)]
  left_bind :: (t -> Either u v) -> Either t v -> Either u v
  left_bind a b =
    case b of
      Left c -> a c
      Right c -> Right c
  mk_list :: Location_0 -> [Expression_0] -> Expression_0
  mk_list l =
    Prelude.foldr
{-
      (\x -> \y ->
        Application_expression_0 (Application_expression_0 (Name_expression_0 (Name l "Construct_List") Nothing []) [x]) [y])
      (Name_expression_0 (Name l "Empty_List") Nothing [])
-}
      (\x -> \y -> Application_expression_0 (Application_expression_0 (Name_expression_0 (Name l "Construct_List")) [x]) [y])
      (Name_expression_0 (Name l "Empty_List"))
  parse :: Parser' t -> (Location_0 -> Location_1) -> String -> Err t
  parse a b c =
    let
       d = parse_error b
    in
      (
        tokenise b c >>=
        \e ->
          case parser a (State e init_location) of
            Left f -> d f
            Right (f, State (Tokens h _) g) ->
              case h of
                [] -> Right f
                _ -> d g)
  parse_alg_pattern :: Parser' Alg_pat
  parse_alg_pattern = parse_op_alg_pattern <+> parse_application_alg_pattern <+> parse_elementary_alg_pattern
  parse_algebraic :: Parser' Data_br_0
  parse_algebraic = Algebraic_data_0 <$ parse_token Algebraic_token <*> parse_optional parse_curlies parse_form
  parse_angulars :: Parser' t -> Parser' t
  parse_angulars a = parse_operator "<" *> a <* parse_operator ">"
  parse_ap_expr :: Parser' Expression_0
  parse_ap_expr = Application_expression_0 <$> parse_br_expr <*> parse_some parse_br_expr
  parse_ap_pat :: Parser' Pat
  parse_ap_pat = Application_pat <$> parse_br_pat <*> parse_some parse_br_pat
  parse_ap_type :: Parser' Type_0
  parse_ap_type = Application_type_0 <$> parse_br_type <*> parse_some parse_br_type
  parse_application_alg_pattern :: Parser' Alg_pat
  parse_application_alg_pattern =
    Application_alg_pat <$> parse_name' <*> ((:) <$> parse_br_alg_pattern <*> parse_many parse_br_alg_pattern)
  parse_application_kind :: Parser' Kind_0
  parse_application_kind = foldl Application_kind_0 <$> parse_bracketed_kind <*> parse_some parse_bracketed_kind
  parse_argument :: Parser' t -> Parser' u -> Parser' (t, u)
  parse_argument p = (<*>) ((<*) ((,) <$> p) parse_colon)
  parse_arguments :: (Parser' [(t, u)] -> Parser' [(t, u)]) -> Parser' t -> Parser' u -> Parser' [(t, u)]
  parse_arguments a b c = parse_optional a (parse_argument b c)
  parse_arguments' :: Parser' t -> Parser' [(t, Type_7)]
  parse_arguments' a = parse_arguments parse_round a parse_type
  parse_arguments'' :: Parser' t -> Parser' [(t, Kind_0)]
  parse_arguments'' a = parse_arguments parse_round a parse_kind
  parse_arrow :: Parser' ()
  parse_arrow = parse_operator "->"
  parse_arrow' :: Parser' (Expression_0 -> t) -> Parser' t
  parse_arrow' p = p <* parse_arrow <*> parse_expression'
  parse_arrow_kind :: Parser' Kind_0
  parse_arrow_kind =
    (
      (\x -> \y -> Application_kind_0 (Application_kind_0 (Name_kind_0 (Name y "Arrow")) x)) <$>
      (parse_application_kind <+> parse_round parse_arrow_kind <+> parse_name_kind) <*>
      parse_arrow_loc <*>
      parse_kind)
  parse_arrow_loc :: Parser' Location_0
  parse_arrow_loc = id <& parse_arrow
  parse_arrow_type :: Parser' Type_0
  parse_arrow_type =
    (
      (\a -> \b -> \c -> Application_type_0 (Name_type_0 (Name b "Arrow") Nothing []) [a, c]) <$>
      parse_br_type' <*>
      (return <&> parse_arrow) <*>
      parse_type')
  parse_basic :: Parser' Def_0
  parse_basic =
    (
      Basic_def_0 <$>
      parse_name'' Def_token <*>
      parse_kt <*>
      parse_constraints <*>
      parse_optional parse_round (first (uncurry Op_pat) <$> parse_pat_and_type) <*
      parse_colon <*>
      parse_type <*
      parse_eq <*>
      parse_expression')
  parse_blank_alg_pattern :: Parser' Alg_pat
  parse_blank_alg_pattern = Blank_alg_pat <$ parse_token Blank_token
  parse_blank_pat :: Parser' Pat
  parse_blank_pat = Blank_pat <& parse_token Blank_token
  parse_br_alg_pattern :: Parser' Alg_pat
  parse_br_alg_pattern = parse_round (parse_op_alg_pattern <+> parse_application_alg_pattern) <+> parse_elementary_alg_pattern
  parse_br_alg_pattern' :: Parser' Alg_pat
  parse_br_alg_pattern' = parse_application_alg_pattern <+> parse_round parse_op_alg_pattern <+> parse_elementary_alg_pattern
  parse_br_expr :: Parser' Expression_0
  parse_br_expr = parse_round (parse_comp_expr <+> parse_ap_expr) <+> parse_elementary_expression
  parse_br_expr' :: Parser' Expression_0
  parse_br_expr' = parse_round parse_comp_expr <+> parse_ap_expr <+> parse_elementary_expression
  parse_br_pat :: Parser' Pat
  parse_br_pat = parse_round (parse_op_pat <+> parse_ap_pat) <+> parse_elementary_pat
  parse_br_pat' :: Parser' Pat
  parse_br_pat' = parse_ap_pat <+> parse_round parse_op_pat <+> parse_elementary_pat
  parse_bracketed_kind :: Parser' Kind_0
  parse_bracketed_kind = parse_round (parse_arrow_kind <+> parse_application_kind) <+> parse_name_kind
  parse_br_type :: Parser' Type_0
  parse_br_type = parse_round (parse_arrow_type <+> parse_ap_type) <+> parse_elementary_type
  parse_br_type' :: Parser' Type_0
  parse_br_type' = parse_round parse_arrow_type <+> parse_ap_type <+> parse_elementary_type
  parse_brackets :: Token_0 -> Parser' t -> Token_0 -> Parser' t
  parse_brackets a b c = parse_token a *> b <* parse_token c
  parse_branch :: Parser' Data_br_0
  parse_branch = Branching_data_0 <& parse_token Branch_token <*> parse_name' <*> parse_optional parse_curlies parse_data_case
  parse_cat :: Parser' Cat_0
  parse_cat =
    (
      Cat_0 <&
      parse_token Cat_token <*>
      parse_curlies (parse_curlies ((,) <$> parse_name' <*> parse_many parse_name')) <*>
      parse_cat_constrs <*>
      parse_round
        (
          (,,,,,,) <$>
          ((,) <&> parse_pat) <*
          parse_arrow <*>
          ((,) <&> parse_pat) <*
          parse_eq <*>
          parse_data_br <*
          parse_comma <*
          parse_name_4 "Compose" <*>
          parse_many parse_br_pat <*
          parse_eq <*>
          parse_expression' <*
          parse_comma <*
          parse_name_4 "Id" <*>
          parse_many parse_br_pat <*
          parse_eq <*>
          parse_expression'))
  parse_cat_constr :: Parser' Name
  parse_cat_constr = parse_token Cat_token *> parse_name'
  parse_cat_constrs :: Parser' [Name]
  parse_cat_constrs = parse_optional (\a -> parse_operator "<<" *> a <* parse_operator ">>") parse_cat_constr
  parse_char :: Parser' Char
  parse_char =
    parse_elementary
      (\a ->
        case a of
          Char_token b -> Just b
          _ -> Nothing)
  parse_char_alg_pattern :: Parser' Alg_pat
  parse_char_alg_pattern = Char_alg_pat <$> parse_char
  parse_char_expression :: Parser' Expression_0
  parse_char_expression = Char_expression_0 <$> parse_char
  parse_class :: Parser' Class_0
  parse_class =
    (
      Class_0 <$>
      parse_name'' Class_token <*>
      parse_kind_vars <*>
      parse_cat_constrs <*>
      parse_curlies ((,) <$> parse_pattern' <* parse_colon <*> parse_kind) <*>
      (Just <$ parse_operator "<" <*> ((,) <$> parse_name' <*> parse_kinds') <* parse_operator ">" <+> pure Nothing) <*>
      parse_optional parse_round parse_method)
  parse_colon :: Parser' ()
  parse_colon = parse_operator "::"
  parse_comma :: Parser' ()
  parse_comma = parse_token Comma_token
  parse_comp_expr :: Parser' Expression_0
{-
  parse_comp_expr = parse_list_expr <+> parse_let_expression <+> parse_match_expression <+> parse_function <+> parse_op_expr
-}
  parse_comp_expr = parse_let_expression <+> parse_match_expression <+> parse_function <+> parse_op_expr
  parse_constraint :: Parser' Constraint_0
  parse_constraint = Constraint_0 <$> parse_name' <*> parse_kinds' <*> parse_name'
  parse_constraints :: Parser' [Constraint_0]
  parse_constraints = parse_optional parse_angulars parse_constraint
  parse_curlies :: Parser' t -> Parser' t
  parse_curlies a = parse_brackets Left_curly_token a Right_curly_token
  parse_data :: Parser' Data_0
  parse_data = Data_0 <& parse_token Data_token <*> parse_name <*> parse_kinds <* parse_eq <*> parse_data_br
  parse_data_br :: Parser' Data_br_0
  parse_data_br = parse_algebraic <+> parse_branch <+> parse_struct
  parse_data_case :: Parser' Data_case_0
  parse_data_case = Data_case_0 <$> parse_name' <*> parse_many parse_name' <* parse_arrow <*> parse_data_br
  parse_def :: Parser' Def_0
  parse_def = parse_basic <+> parse_instance
  parse_elementary :: (Token_0 -> Maybe t) -> Parser' t
  parse_elementary a =
    Parser
      (\(State (Tokens b c) d) ->
        case b of
          [] -> Left c
          Token_1 e f : g ->
            let
              h = max d e
            in
              case a f of
                Just i -> Right (i, (State (Tokens g c) h))
                Nothing -> Left h)
  parse_elementary_alg_pattern :: Parser' Alg_pat
  parse_elementary_alg_pattern =
    parse_blank_alg_pattern <+> parse_char_alg_pattern <+> parse_int_alg_pattern <+> parse_name_alg_pattern
  parse_elementary_expression :: Parser' Expression_0
  parse_elementary_expression =
{-
    (
      parse_char_expression <+>
      parse_int_expression <+>
      (\x -> Name_expression_0 (Name x "Empty_List") Nothing []) <& parse_name_4 "List" <+>
      parse_name_expression)
-}
    parse_char_expression <+> parse_int_expression <+> parse_list_expr <+> parse_name_expression
  parse_elementary_pat :: Parser' Pat
  parse_elementary_pat = parse_blank_pat <+> parse_name_pat
  parse_elementary_type :: Parser' Type_0
  parse_elementary_type = parse_name_type <+> (int_to_nat_type_0 <&> parse_int')
  parse_eq :: Parser' ()
  parse_eq = parse_operator "="
  parse_error :: (Location_0 -> Location_1) -> Location_0 -> Err t
  parse_error a b = Left ("Parse error" ++ location' (a b))
  parse_expression :: String -> Err Expression_0
  parse_expression = parse parse_expression' (Location_1 "input")
  parse_expression' :: Parser' Expression_0
  parse_expression' = parse_comp_expr <+> parse_ap_expr <+> parse_elementary_expression
  parse_form :: Parser' Form_0
  parse_form = Form_0 <$> parse_name' <*> parse_many (Type_7 <&> parse_br_type)
  parse_function :: Parser' Expression_0
  parse_function = parse_arrow' (Function_expression_0 <$> parse_pat)
  parse_instance :: Parser' Def_0
  parse_instance =
    (
      Instance_def_0 <&
      parse_token Instance_token <*>
      parse_kind_vars <*>
      parse_cat_constrs <*>
      parse_name' <*>
      parse_kinds' <*>
      parse_curlies
        (
          (,,,) <$>
          parse_name' <*>
          parse_optional' (Just <$> parse_curlies (parse_curlies parse_kind)) <*>
          parse_optional (parse_sq <$> parse_sq) parse_kind <*>
          parse_many parse_pattern_1) <*>
      parse_constraints <*>
      parse_optional parse_round ((,) <$> parse_pat <* parse_eq <*> parse_expression'))
  parse_int :: Parser' Integer
  parse_int =
    parse_int' <+> (negate <$ parse_operator "-" <*> parse_int' >>= \x -> if x == 0 then empty_parser else return x)
  parse_int' :: Parser' Integer
  parse_int' =
    parse_elementary
      (\a ->
        case a of
          Int_token b -> Just b
          _ -> Nothing)
  parse_int_alg_pattern :: Parser' Alg_pat
  parse_int_alg_pattern = Int_alg_pat <$> parse_int
  parse_int_expression :: Parser' Expression_0
  parse_int_expression = Int_expression_0 <$> parse_int
  parse_let_expression :: Parser' Expression_0
  parse_let_expression =
    (
      flip (Prelude.foldr Let_expression_0) <$
      parse_token Let_token <*>
      parse_list 1 ((,) <$> parse_pat <* parse_eq <*> parse_expression') <*
      parse_token In_token <*>
      parse_expression')
  parse_kind :: Parser' Kind_0
  parse_kind = parse_arrow_kind <+> parse_application_kind <+> parse_name_kind
  parse_kind_vars :: Parser' [Name]
  parse_kind_vars = parse_optional (parse_sq <$> parse_sq) parse_name'
  parse_kinds :: Parser' [(Name, Kind_0)]
  parse_kinds = parse_arguments parse_sq parse_name' parse_kind
  parse_kinds' :: Parser' [Kind_0]
  parse_kinds' = parse_optional (parse_sq <$> parse_sq) parse_kind
  parse_kt :: Parser' KT0
  parse_kt = KT0 <$> parse_kind_vars <*> parse_cat_constrs <*> parse_kinds
  parse_list :: Integer -> Parser' t -> Parser' [t]
  parse_list i p =
    case i of
      0 -> parse_list 1 p <+> return []
      1 -> (:) <$> p <*> parse_many (parse_comma *> p)
      _ -> (:) <$> p <* parse_comma <*> parse_list (i - 1) p
  parse_list_expr :: Parser' Expression_0
{-
  parse_list_expr = mk_list <& parse_name_4 "List" <*> parse_round (parse_list 1 parse_expression')
-}
  parse_list_expr = mk_list <&> parse_sq (parse_list 1 parse_expression')
  parse_load :: Parser' Name
  parse_load = parse_name_3 Load_token ((flip (++) ".awf" <$> parse_name) <* parse_operator "." <* parse_name_4 "awf")
  parse_location :: Parser' Location_0
  parse_location = Parser (\a -> Right (state_location a, a))
  parse_many :: Parser' t -> Parser' [t]
  parse_many a = parse_some a <+> return []
  parse_match_expression :: Parser' Expression_0
  parse_match_expression =
    (
      Match_expression_0 <&
      parse_token Match_token <*>
      parse_expression' <*>
      parse_curlies (parse_list 0 (parse_arrow' ((,) <$> parse_alg_pattern))))
  parse_method :: Parser' Method
  parse_method = Method <$> parse_name' <*> parse_kinds <*> parse_constraints <* parse_colon <*> parse_type
  parse_name :: Parser' String
  parse_name =
    parse_elementary
      (\a ->
        case a of
          Name_token b -> Just b
          _ -> Nothing)
  parse_name' :: Parser' Name
  parse_name' = Name <&> parse_name
  parse_name'' :: Token_0 -> Parser' Name
  parse_name'' = flip parse_name_3 parse_name
  parse_name_3 :: Token_0 -> Parser' String -> Parser' Name
  parse_name_3 a b = Name <& parse_token a <*> b
  parse_name_4 :: String -> Parser' ()
  parse_name_4 = parse_token <$> Name_token
  parse_name_alg_pattern :: Parser' Alg_pat
  parse_name_alg_pattern =
    (
      parse_name' >>=
      \(Name a (b : c)) ->
        case (isUpper b, isLower b) of
          (False, False) -> empty_parser
          (False, True) -> return (Name_alg_pat (Name a (b : c)))
          (True, _) -> return (Application_alg_pat (Name a (b : c)) []))
  parse_name_expression :: Parser' Expression_0
{-
  parse_name_expression =
    (
      Name_expression_0 <$>
      parse_name' <*>
      parse_optional' (Just <$> parse_curlies parse_type) <*>
      parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_type) Right_square_token))
-}
  parse_name_expression = Name_expression_0 <$> parse_name'
  parse_name_kind :: Parser' Kind_0
  parse_name_kind = Name_kind_0 <$> parse_name'
  parse_name_pat :: Parser' Pat
  parse_name_pat =
    (
      parse_name' >>=
      \(Name a (b : c)) ->
        case (isUpper b, isLower b) of
          (False, False) -> empty_parser
          (False, True) -> return (Name_pat (Name a (b : c)))
          (True, _) -> return (Constr_pat (Name a (b : c))))
  parse_name_type :: Parser' Type_0
  parse_name_type =
    (
      Name_type_0 <$>
      parse_name' <*>
      parse_optional' (Just <$> parse_curlies (parse_curlies parse_kind)) <*>
      parse_optional (parse_sq <$> parse_sq) parse_kind)
  parse_op :: Parser' String
  parse_op =
    parse_elementary
      (\a ->
        case a of
          Operator_token b -> Just b
          _ -> Nothing)
  parse_op_0 :: Parser' String
  parse_op_0 = filter_parser (\x -> notElem x ["->", "::", "="]) parse_op
  parse_op_0' :: Parser' Name
  parse_op_0' = Name <&> parse_op_0
  parse_op_alg_pattern :: Parser' Alg_pat
  parse_op_alg_pattern = Op_alg_pat <$> parse_br_alg_pattern' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_alg_pattern')
  parse_op_expr :: Parser' Expression_0
  parse_op_expr = Op_expression_0 <$> parse_br_expr' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_expr')
  parse_op_pat :: Parser' Pat
  parse_op_pat = Op_pat <$> parse_br_pat' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_pat')
  parse_opdecl :: Parser' Opdecl_0
  parse_opdecl =
    (
      Opdecl_0 <&
      parse_token Opdecl_token <*>
      parse_op_0 <*>
      parse_name <*>
      parse_int <*>
      (Lft <$ parse_name_4 "Left" <+> Rght <$ parse_name_4 "Right"))
  parse_operator :: String -> Parser' ()
  parse_operator x = parse_token (Operator_token x)
  parse_optional :: (Parser' [t] -> Parser' [t]) -> Parser' t -> Parser' [t]
  parse_optional a b = parse_optional' (a (parse_list 1 b))
  parse_optional' :: Alternative f => Parser' (f t) -> Parser' (f t)
  parse_optional' a = a <+> pure empty
  parse_pat :: Parser' Pat
  parse_pat = parse_op_pat <+> parse_ap_pat <+> parse_elementary_pat
  parse_pat_and_type :: Parser' ((Pat, [(Name, Pat)]), Type_7)
  parse_pat_and_type =
    (
      (\a -> first ((,) a)) <$>
      parse_br_pat' <*>
      (
        (,) [] <$ parse_colon <*> parse_type <+>
        (
          (\a -> first (\(b, c) -> (a, b) : c)) <$>
          (Name <&> parse_op_0) <*>
          parse_pat_and_type)))
  parse_pattern_1 :: Parser' Pattern_1
  parse_pattern_1 = Blank_pattern_1 <$ parse_token Blank_token <+> Name_pattern_1 <$> parse_name'
  parse_pattern' :: Parser' Name
  parse_pattern' = Name <&> ("_" <$ parse_token Blank_token <+> parse_name)
  parse_round :: Parser' t -> Parser' t
  parse_round a = parse_brackets Left_round_token a Right_round_token
  parse_some :: Parser' t -> Parser' [t]
  parse_some a = (:) <$> a <*> parse_many a
  parse_sq :: Parser' t -> Parser' t
  parse_sq a = parse_brackets Left_square_token a Right_square_token
  parse_struct :: Parser' Data_br_0
  parse_struct = Struct_data_0 <$ parse_token Struct_token <*> parse_name' <*> parse_arguments' parse_name'
  parse_token :: Token_0 -> Parser' ()
  parse_token a = parse_elementary (\b -> if b == a then Just () else Nothing)
  parse_tree :: (Location_0 -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser' Tree_1
  parse_tree' =
    (
      Tree_1 <$>
      parse_many parse_load <*>
      (
        Tree_0 <$>
        parse_many parse_data <*>
        parse_many parse_cat <*>
        parse_many parse_class <*>
        parse_many parse_opdecl <*>
        parse_many parse_def))
  parse_type :: Parser' Type_7
  parse_type = Type_7 <&> parse_type'
  parse_type' :: Parser' Type_0
  parse_type' = parse_arrow_type <+> parse_ap_type <+> parse_elementary_type
  state_location :: State -> Location_0
  state_location (State (Tokens a b) _) =
    case a of
      [] -> b
      Token_1 c _ : _ -> c
  update_location :: State -> Location_0 -> State
  update_location (State a b) c = State a (max b c)
--------------------------------------------------------------------------------------------------------------------------------