--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tokenise (
  Err,
  Location_0 (..),
  Location_1 (..),
  Token_0 (..),
  Tokens (..),
  current_line_and_char,
  location,
  location',
  nom_token,
  tokenise) where
  import Data.Bifunctor
  import Data.Char
  data Char' =
    Delimiter_char Delimiter |
    Int_char Char |
    Left_curly_char |
    Minus_char |
    Name_char Char |
    Newline_char |
    Operator_char Char |
    Right_curly_char |
    Space_char
      deriving Show
  data Delimiter =
    Comma_delimiter | Left_round_delimiter | Left_square_delimiter | Right_round_delimiter | Right_square_delimiter
      deriving Show
  type Err = Either String
  data Location_0 = Location_0 Integer Integer deriving (Eq, Ord, Show)
  data Location_1 = Location_1 String Location_0 deriving Show
  data Token_0 =
    Algebraic_token |
    Blank_token |
    Branch_token |
    Cat_token |
    Class_token |
    Comma_token |
    Data_token |
    Def_token |
    In_token |
    Infixl_token |
    Infixr_token |
    Instance_token |
    Int_token Integer |
    Left_curly_token |
    Left_round_token |
    Left_square_token |
    Let_token |
    Load_token |
    Match_token |
    Name_token String |
    Negate_token |
    Of_token |
    Operator_token String |
    Right_curly_token |
    Right_round_token |
    Right_square_token |
    Struct_token
      deriving (Eq, Show)
  data Token_1 = Token_1 Location_0 Token_0 deriving Show
  data Tokens = Tokens [Token_1] Location_0 deriving Show
  accumulate :: (String -> Token_0) -> (Char' -> Maybe Char) -> Location_1 -> [Char'] -> Err Tokens
  accumulate a b c d = (\(e, f) -> add_token c (a e) f) <$> accumulate' b c d
  accumulate' :: (Char' -> Maybe Char) -> Location_1 -> [Char'] -> Err (String, Tokens)
  accumulate' a b c =
    let
      d = (,) "" <$> tokenise' b c
    in
      case c of
        [] -> d
        e : f ->
          case a e of
            Just g -> first ((:) g) <$> accumulate' a (next_char b) f
            Nothing -> d
  add_token :: Location_1 -> Token_0 -> Tokens -> Tokens
  add_token (Location_1 _ a) b (Tokens c d) = Tokens (Token_1 a b : c) d
  char :: Char -> Char'
  char a =
    case a of
      '\n' -> Newline_char
      ' ' -> Space_char
      '(' -> Delimiter_char Left_round_delimiter
      ')' -> Delimiter_char Right_round_delimiter
      ',' -> Delimiter_char Comma_delimiter
      '-' -> Minus_char
      '[' -> Delimiter_char Left_square_delimiter
      ']' -> Delimiter_char Right_square_delimiter
      '{' -> Left_curly_char
      '}' -> Right_curly_char
      _ ->
        (if_sequence
          a
          [
            (
              flip
                elem
                ['!', '"', '#', '$', '%', '&', '*', '+', '.', '/', ':', ';', '|', '<', '=', '>', '?', '@', '\\', '^', '`', '~'],
              Operator_char),
            (isDigit, Int_char)]
          Name_char)
            a
  current_line_and_char :: Tokens -> Location_0
  current_line_and_char (Tokens tokens line_and_char_1) =
    case tokens of
      [] -> line_and_char_1
      Token_1 line_and_char_0 _ : _ -> line_and_char_0
  end_tokens :: Location_1 -> Err Tokens
  end_tokens (Location_1 _ a) = Right (Tokens [] a)
  file_location :: Location_1 -> String
  file_location (Location_1 a _) = a
  if_sequence :: t -> [(t -> Bool, u)] -> u -> u
  if_sequence a b c =
    case b of
      [] -> c
      (d, e) : f -> if d a then e else if_sequence a f c
  int_char :: Char' -> Maybe Char
  int_char a =
    case a of
      Int_char b -> Just b
      _ -> Nothing
  location :: Location_1 -> String
  location (Location_1 a b) = " at " ++ a ++ ":" ++ location0 b
  location' :: Location_1 -> String
  location' a = location a ++ "."
  location0 :: Location_0 -> String
  location0 (Location_0 a b) = show a ++ ":" ++ show b
  name_char :: Char' -> Maybe Char
  name_char a =
    case a of
      Int_char b -> Just b
      Name_char b -> Just b
      _ -> Nothing
  next_char :: Location_1 -> Location_1
  next_char (Location_1 a (Location_0 b c)) = Location_1 a (Location_0 b (c + 1))
  next_line :: Location_1 -> Location_1
  next_line (Location_1 a (Location_0 b _)) = Location_1 a (Location_0 (b + 1) 1)
  nom_token :: (Token_0 -> Maybe t) -> Tokens -> Either Location_0 (t, Tokens)
  nom_token f (Tokens tokens_0 line_and_char_1) =
    case tokens_0 of
      [] -> Left line_and_char_1
      Token_1 line_and_char_0 token : tokens_1 ->
        case f token of
          Nothing -> Left line_and_char_0
          Just x -> Right (x, Tokens tokens_1 line_and_char_1)
  operator_char :: Char' -> Maybe Char
  operator_char a =
    case a of
      Minus_char -> Just '-'
      Operator_char b -> Just b
      _ -> Nothing
  tokenise :: (Location_0 -> Location_1) -> String -> Err Tokens
  tokenise a b = tokenise' (a (Location_0 1 1)) (char <$> b)
  tokenise' :: Location_1 -> [Char'] -> Err Tokens
  tokenise' a b =
    case b of
      [] -> end_tokens a
      c : d ->
        let
          e = next_char a
          f = tokenise_operator a b
        in
          case c of
            Delimiter_char g ->
              (
                add_token
                  a
                  (case g of
                    Comma_delimiter -> Comma_token
                    Left_round_delimiter -> Left_round_token
                    Left_square_delimiter -> Left_square_token
                    Right_round_delimiter -> Right_round_token
                    Right_square_delimiter -> Right_square_token) <$>
                tokenise' e d)
            Int_char _ -> accumulate (Int_token <$> read) int_char a b
            Left_curly_char ->
              case d of
                Minus_char : x -> tokenise_multiline 1 (next_char e) x
                _ -> add_token a Left_curly_token <$> tokenise' e d
            Minus_char ->
              case d of
                Int_char _ : _ -> add_token a Negate_token <$> tokenise' e d
                Minus_char : x -> tokenise_single (next_char e) x
                _ -> f
            Name_char _ -> accumulate word_token name_char a b
            Newline_char -> tokenise' (next_line a) d
            Operator_char _ -> f
            Right_curly_char -> add_token a Right_curly_token <$> tokenise' e d
            Space_char -> tokenise' e d
  tokenise_l :: Integer -> Location_1 -> [Char'] -> Err Tokens
  tokenise_l a b c =
    case c of
      Minus_char : d -> tokenise_multiline (a + 1) (next_char b) d
      _ -> tokenise_multiline a b c
  tokenise_m :: Integer -> Location_1 -> [Char'] -> Err Tokens
  tokenise_m a b c =
    case c of
      Right_curly_char : d ->
        (case a of
          1 -> tokenise'
          _ -> tokenise_multiline (a - 1))
            (next_char b)
            d
      _ -> tokenise_multiline a b c
  tokenise_multiline :: Integer -> Location_1 -> [Char'] -> Err Tokens
  tokenise_multiline a b c =
    case c of
      [] -> Left ("Missing end comment in " ++ file_location b ++ ".")
      d : e ->
        let
          f = tokenise_multiline a
          g = next_char b
        in
          case d of
            Left_curly_char -> tokenise_l a g e
            Minus_char -> tokenise_m a g e
            Newline_char -> f (next_line b) e
            _ -> f g e
  tokenise_operator :: Location_1 -> [Char'] -> Err Tokens
  tokenise_operator = accumulate Operator_token operator_char
  tokenise_single :: Location_1 -> [Char'] -> Err Tokens
  tokenise_single a b =
    case b of
      [] -> end_tokens a
      c : d ->
        (case c of
          Newline_char -> tokenise' (next_line a)
          _ -> tokenise_single (next_char a))
            d
  word_token :: String -> Token_0
  word_token a =
    case a of
      "_" -> Blank_token
      "Cat" -> Cat_token
      "algebraic" -> Algebraic_token
      "branch" -> Branch_token
      "case" -> Match_token
      "class" -> Class_token
      "data" -> Data_token
      "def" -> Def_token
      "import" -> Load_token
      "in" -> In_token
      "infixl" -> Infixl_token
      "infixr" -> Infixr_token
      "instance" -> Instance_token
      "let" -> Let_token
      "of" -> Of_token
      "struct" -> Struct_token
      _ -> Name_token a
--------------------------------------------------------------------------------------------------------------------------------