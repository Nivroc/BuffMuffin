module Main where

import Lib
import Data.Configurator
import Control.Concurrent
import Control.Exception


main :: IO ()
main = do
    let cfgLoad = autoReload autoConfig [Required "/tmp/config/application.cfg"]
    bracket cfgLoad (killThread . snd) (startUp . fst) --startUp
