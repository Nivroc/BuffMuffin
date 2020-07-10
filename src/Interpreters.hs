{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreters where

import           Relude 
import           Model              as M
import           Conditions
import           Control.Lens
import           Calamity
import qualified Polysemy           as P
import qualified Polysemy.Output    as P
import qualified Polysemy.Input     as P
import           Data.Time.Clock
import           Data.Configurator.Types

outToFile :: (P.Member (P.Embed IO) r) => FilePath -> (P.Sem (P.Output Message : r) a) -> P.Sem r a
outToFile path = P.interpret \case 
    P.Output mess -> 
        let time = mess ^. #timestamp
            content = mess ^. #content
        in P.embed $ appendFile (path <> (show $ utctDay time) <> ".txt") ("\n" <> (toString content))

inputFromFile :: (P.Member (P.Embed IO) r) => FilePath -> (P.Sem (P.Input Text : r) a) -> P.Sem r a
inputFromFile path = P.interpret \case 
    P.Input -> do time <- P.embed getCurrentTime    
                  result <- P.embed $ readFile (path <> (show $ utctDay time) <> ".txt") 
                  return $ toText result

appConfigFrom :: AppConfig -> P.Sem (GetConfig : r) a -> P.Sem r a
appConfigFrom cfg = P.interpret \case M.GetConfig -> pure cfg
