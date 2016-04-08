module Main where

import Eval

main :: IO ()
main = print $ eval "[+,*,~2,K:1]:<3,5,6>"
