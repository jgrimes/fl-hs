module Main where

import Eval

main :: IO ()
main = eval "[+,*,~2,K:1]:<3,5,6>"
