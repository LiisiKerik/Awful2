--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
import Classes_0
import Data.Bifunctor
import Data.List
import Data.Map
import Datas_0
import Eval
import Naming
import Standard
import System.Directory
import System.Environment
import System.FilePath
import Tokenise
import Tree
import Typing
type Files = Map' (File, Map' Op)
check ::
  (
    [String] ->
    (
      Files,
      (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
      Map' Expression_2,
      Map' ([String], Map' [(String, Nat)])) ->
    Location' ->
    String ->
    IO
      (Err
        (
          (
            Files,
            (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
            Map' Expression_2,
            Map' ([String], Map' [(String, Nat)])),
          (File, Map' Op))))
check b m' @ (f, _, _, _) j name_qc =
  case Data.Map.lookup name_qc f of
    Just a -> return (Right (m', a))
    Nothing ->
      case check' name_qc b of
        Just a -> return (Left ("Circular dependency between files [" ++ intercalate ", " a ++ "]."))
        Nothing -> do
          find_file <- findFile [""] name_qc
          case find_file of
            Just file -> do
              a <- readFile file
              case parse_tree (Location_1 name_qc) a of
                Left c -> return (Left c)
                Right (Tree_1 c d) -> do
                  g <-
                    check_imports
                      (name_qc : b)
                      (m', init_type_context)
                      ((\(Name h i) -> (Library (Location_1 name_qc h), i)) <$> c)
                  return
                    (
                      g >>=
                      \((h, i, l, p'), m) ->
                        (
                          (\(k, n, o, s) -> ((Data.Map.insert name_qc n h, k, o, s), n)) <$>
                          standard_naming_typing name_qc d (i, m, l, p')))
            Nothing ->
              err
                (
                  "Failed to find file " ++
                  name_qc ++
                  " requested" ++
                  case j of
                    Language -> " in the command."
                    Library k -> location' k)
check' :: String -> [String] -> Maybe [String]
check' a b =
  case b of
    [] -> Nothing
    c : d -> if c == a then Just [a] else (:) c <$> check' a d
check_extension :: String -> Err ()
check_extension a =
  case takeExtension a of
    ".awf" -> Right ()
    _ -> Left ("File " ++ a ++ " has an invalid extension. You can only use a .awf file.")
check_extensions :: String -> [String] -> Err ([String], String)
check_extensions a c =
  case c of
    [] -> Right ([], a)
    d : e -> check_extension a >> first ((:) a) <$> check_extensions d e
check_imports ::
  (
    [String] ->
    (
      (
        Files,
        (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
        Map' Expression_2,
        Map' ([String], Map' [(String, Nat)])),
      (File, Map' Op)) ->
    [(Location', String)] ->
    IO
      (Err
        (
          (
            Files,
            (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
            Map' Expression_2,
            Map' ([String], Map' [(String, Nat)])),
          (File, Map' Op))))
check_imports a b @ (f, k) c =
  case c of
    [] -> return (Right b)
    (d, g) : e -> do
      x <- check a f d g
      case x of
        Left i -> err i
        Right (i, n) -> check_imports a (i, context_union k n) e
err :: String -> IO (Err t)
err = return <$> Left
eval'' :: [String] -> String -> IO (Err String)
eval'' a b = do
  c <- check_imports [] (init', init_type_context) ((,) Language <$> a)
  return
    (
      c >>=
      \((_, (t, _, _, _), f, y), (File j g i w _ _ _ m _ d2 w' x', u)) ->
        tokenise_parse_naming_typing_eval t j (d2, x', g, i) f b m y w u w')
init' ::
  (
    Files,
    (Map' Location', Map' Location', Map' Location', Map' (Map' Location')),
    Map' Expression_2,
    Map' ([String], Map' [(String, Nat)]))
init' =
  (
    empty,
    (
      locations,
      (fromList ((\x -> (x, Language)) <$> ["->", "::", "="])),
      singleton "Star" Language,
      fromList [("Ord", fromList [("Int", Language)]), ("Ring", fromList [("Int", Language)])]),
    defs,
    fromList
      [("Ord", (["compare"], fromList [("Int", [])])), ("Ring", (["add", "convert", "multiply"], fromList [("Int", [])]))])
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Missing command."
    command : arguments ->
      case command of
        "Check" ->
          case arguments of
            [f] ->
              case check_extension f of
                Left a -> putStrLn a
                _ -> do
                  res <- check [] init' Language f
                  putStrLn
                    (case res of
                      Left e -> e
                      _ -> "Library check successful!")
            _ -> putStrLn "Command check expects 1 argument."
        "Eval" ->
          case arguments of
            a : b ->
              case check_extensions a b of
                Left e -> putStrLn e
                Right (c, d) -> do
                  e <- eval'' c d
                  putStrLn
                    (case e of
                      Left f -> f
                      Right f -> f)
            _ -> putStrLn "Command eval expects at least 1 argument."
        _ -> putStrLn ("Invalid command " ++ command ++ ".")
--------------------------------------------------------------------------------------------------------------------------------