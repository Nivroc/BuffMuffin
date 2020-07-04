{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}




module Lib where

import           Relude 

import           Calamity
import           Calamity.Metrics.Noop
import           Calamity.Cache.InMemory
import           Calamity.Commands
import qualified Calamity.Commands.Context                  as CommandContext
import           Calamity.Types.Model.User

import qualified Polysemy                                   as P
import qualified Polysemy.Reader                            as P
import qualified Polysemy.Error                             as P

import           Control.Lens
import           Control.Concurrent.MVar
import           Control.Conditional (condDefault)
import           Control.Monad
import           Control.Lens.Extras
import           Control.Lens.Operators

import qualified Data.Text                                  as E
import           Data.Complex
import           Data.Default.Class
import           Data.Monoid

import           Text.RE.TDFA.Text                          as R

data AppConfig = AppConfig {
    myID :: Snowflake User,
    sources :: [Snowflake Channel],
    controls :: [Snowflake Channel],
    output :: Snowflake Channel,
    plusKeys :: [Text],
    minusKeys :: [Text],
    specPlus :: [R.RE],
    specMin :: [R.RE],
    password :: Text
}

appConfigFrom :: P.Sem (GetConfig : r) a -> P.Sem r a
appConfigFrom = P.interpret \case GetConfig -> pure cfg 
    where cfg = AppConfig {
        myID = getID ("725657610854072370"::Text),
        sources = getID <$> ["668419602337759253" :: Text, "702560043022811136", "668419781241864192", "725657927033028672"],
        controls = getID <$> ["725657927033028672" :: Text, "711192812682739802"],
        output = getID ("711192812682739802"::Text),
        plusKeys = [],
        minusKeys = ["?", "when", "anyone", "any", "not sure", "unsure"],
        specPlus = [[R.re|\d?\d[.]?\s?\d\d|]],
        specMin = [[R.re|Is.*|]],
        password = "havana123"
    }
{-
data Retranslator = Retr { from :: MVar [Channel], to :: MVar Channel}
data GetRetr m a where
    GetRetranslator :: GetRetr m Retranslator
    -}

data GetConfig m a where
    GetConfig :: GetConfig m AppConfig

P.makeSem ''GetConfig

coerceSF txt = case reads (toString txt) :: [(Word64,String)] of [(flake, "")] -> Snowflake flake  
instance HasID Channel Text where
    getID = coerceSF
instance HasID User Text where
    getID = coerceSF 

tellToId :: forall msg r t. (BotC r) => Snowflake Channel -> Text -> P.Sem r (Either RestError Message)
tellToId cid msg = P.runError $ do
  r <- invoke $ CreateMessage (prnt cid) (#content .~ (Just msg) $ def)
  P.fromEither r
  where prnt flake = (show $ fromSnowflake $ flake) :: Text

someFunc :: IO ()
someFunc = void . P.runFinal . P.embedToFinal . runCacheInMemory . runMetricsNoop . useConstantPrefix "muffin " . appConfigFrom $ 
    runBotIO (UserToken "NzI1NjU3NjEwODU0MDcyMzcw.XvR7kA.b3ucHp7b186QOnnSYtgjYRBUKFE") $ 
    do  conf <- getConfig
        react @'MessageCreateEvt $ \msg -> 
            do parsePrefix msg >>= \case
                Just (prefix, cmd) -> respondToCommand msg cmd
                Nothing -> do 
                    print $ "Not a command: " <> show msg
                    when (getID @User msg /= myID conf) (void $ retranslateOrPass msg (output conf) (sources conf))
    

respondToCommand :: (BotC r, Tellable msg, P.Member GetConfig r, HasID Channel msg) => msg -> LText -> P.Sem r ()
respondToCommand msg command = getConfig >>= \conf -> 
    let 
        secCommands cmd pwd = when (pwd == (password conf))  
                            (case cmd of
                                "ping" -> void $ tell msg ("pong" :: Text)
                                "goodbye" -> void $ tell msg ("goodbye" :: Text) >> stopBot)
        publicCommands cmd = case cmd of
                                "hello" -> void $ tell msg ("Hey there ;)" :: Text)
                                "heal" -> void $ tell msg ("HEAL NIVROC!!!111" :: Text)
                                "help" -> void $ tell msg ("Only God can help you..." :: Text)
                                _ -> void $ tell msg ("Hey, I don't know( Maybe you can teach me?" :: Text)                  
    in do
        let (cmd, pwd) = case words $ toText command of
                            (cmd : pwd : []) -> (cmd, pwd)
                            (cmd : []) -> (cmd, "")
                            _ -> ("", "")
        when (getID @Channel msg `elem` (controls conf) )
             (if (E.null pwd) then publicCommands cmd else secCommands cmd pwd)



retranslateOrPass :: (BotC r, P.Member GetConfig r) => Message -> Snowflake Channel -> [Snowflake Channel] -> P.Sem r ()
retranslateOrPass msg output sources = getConfig >>= \conf -> 
    let redAlert = correctSource 
                   && (specialPlus || (getAny $ mconcat $ (Any . ((flip E.isInfixOf) (E.toCaseFold message)) . E.toCaseFold) <$> (plusKeys conf)))
                   && not (specialMinus || (getAny $ mconcat $ (Any . ((flip E.isInfixOf) (E.toCaseFold message)) . E.toCaseFold) <$> (minusKeys conf)))
        special lst = getAny $ mconcat $ (Any . matched . (message ?=~)) <$> lst
        specialPlus = special $ specPlus conf
        specialMinus = special $ specMin conf
    in
    condDefault (pure ()) [
        (redAlert, alert $ "Alert: " <> message)
    ]
    where correctSource = getID @Channel msg `elem` sources
          message = (toText $ msg ^. #content)
          alert mess = void $ tellToId output mess





                        

