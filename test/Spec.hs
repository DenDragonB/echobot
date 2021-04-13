module Main where

import qualified TestBot
import qualified TestVK
import qualified TestTG

main :: IO ()
main = do
    TestBot.main
    TestVK.main
    TestTG.main
