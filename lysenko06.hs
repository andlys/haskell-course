{-# OPTIONS_GHC -Wall #-}
module Main where
import System.IO
import System.Environment

main :: IO ()
main = do args <- getArgs
          if null args then
            do contents <- hGetContents stdin
               hPutStr stdout $ seqSymb contents
          else
            do let fname = args!!0
               contents <- readFile fname
               hPutStr stdout $ seqSymb contents

seqSymb :: String -> String
seqSymb [] = []
seqSymb str@(x:_) = let substr = takeWhile (== x) str
                        len = length substr
                    in (if len > 3
                        then "(" ++ show len ++ ")" ++ [x]
                        else substr) ++ (seqSymb $ dropWhile (== x) str)

{-
--test
seqSymb "" == ""
seqSymb "a" == "a"
seqSymb "aa" == "aa"
seqSymb "aaa" == "aaa"
seqSymb "aaaaa" == "(5)a"
seqSymb "     " == "(5) "
seqSymb "aaa b bbbb sssss cc" == "aaa b (4)b (5)s cc"
seqSymb "a ==== //// b" == "a (4)= (4)/ b"
-}
