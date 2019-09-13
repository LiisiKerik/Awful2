--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Trace where
  import System.IO
  import System.IO.Unsafe
  infixl 0 .!.
  (.!.) :: Show t => String -> t -> String
  (.!.) = show_application_brackets
  infixl 0 ...
  (...) :: Show t => String -> t -> String
  (...) = show_application
  {-# NOINLINE handle #-}
  handle :: Handle
  handle = unsafePerformIO (openFile "Trace.txt" AppendMode)
  show_application :: Show t => String -> t -> String
  show_application s x = s ++ " " ++ show x
  show_application_brackets :: Show t => String -> t -> String
  show_application_brackets s x = s ++ " (" ++ show x ++ ")"
  {-# NOINLINE trace #-}
  trace :: Show t => String -> t -> t
  trace s x =
    let
      t = s ++ "\n\n" ++ show x ++ "\n\n\n"
    in
      unsafePerformIO
        (do
          _ <- return $! length t
          hPutStr handle t
          hFlush handle
          return x)
--------------------------------------------------------------------------------------------------------------------------------