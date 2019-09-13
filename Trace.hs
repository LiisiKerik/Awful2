--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Trace where
  import System.IO.Unsafe
  infixl 0 .!.
  (.!.) :: Show t => String -> t -> String
  (.!.) = show_application_brackets
  infixl 0 ...
  (...) :: Show t => String -> t -> String
  (...) = show_application
  {-# NOINLINE trace #-}
  trace :: Show t => String -> t -> t
  trace s x = return x $! unsafePerformIO (appendFile "trace.txt" (s ++ "\n\n" ++ show x ++ "\n\n\n"))
  show_application :: Show t => String -> t -> String
  show_application s x = s ++ " " ++ show x
  show_application_brackets :: Show t => String -> t -> String
  show_application_brackets s x = s ++ " (" ++ show x ++ ")"
--------------------------------------------------------------------------------------------------------------------------------