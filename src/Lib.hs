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
import           Conditions
import qualified Interpreters                               as I
import qualified Model                                      as M

import           Calamity
import           Calamity.Metrics.Noop
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.HTTP.Channel                      as C

import qualified Polysemy                                   as P
import qualified Polysemy.Output                            as P
import qualified Polysemy.Error                             as P
import qualified Polysemy.Input                             as P

import           Control.Lens
import           Control.Conditional (condDefault)
import           Control.Monad
import           Control.Lens.Extras
import           Control.Lens.Operators
import           Control.Arrow

import qualified Data.Text                                  as E
import           Data.Default.Class
import           Data.Monoid
import           Data.Time.Clock
import           Data.Configurator.Types
import           Data.Configurator

import           Text.RE.TDFA.Text                          as R


tellToId :: forall msg r t. (BotC r) => 
    Snowflake Channel -> Text -> Maybe C.AllowedMentions -> P.Sem r (Either RestError Message)
tellToId cid msg roles = P.runError $ do
  let messopts = #content .~ (Just msg) $ def 
  let withroles = #allowedMentions .~ roles $ messopts
  r <- invoke $ CreateMessage (prnt cid) messopts
  P.fromEither r
  where prnt flake = (show $ fromSnowflake flake) :: Text


startUp :: Config -> IO ()
startUp cfg = do
    token <- require @LText cfg "usertoken"
    logpath <- lookupDefault @FilePath "/tmp/logs/" cfg "logpath"
    botPrefix <- lookupDefault @LText "muffin " cfg "prefix"
    appConfig <- M.fromCfgFile cfg
    void . P.runFinal . P.embedToFinal . runCacheInMemory . runMetricsNoop . 
            I.outToFile logpath . I.inputFromFile logpath . useConstantPrefix botPrefix . I.appConfigFrom appConfig $ 
            runBotIO (UserToken token) $ 
        do  conf <- M.getConfig
            react @'MessageCreateEvt $ \msg -> 
                parsePrefix msg >>= \case
                    Just (prefix, cmd) -> respondToCommand msg cmd
                    Nothing -> do 
                        print $ "Not a command: " <> show msg
                        when (getID @User msg /= M.myID conf) (void $ retranslateOrPass msg (M.output conf))
    

respondToCommand :: (BotC r, Tellable msg, P.Member M.GetConfig r, P.Member (P.Input Text) r, HasID Channel msg) => 
                    msg -> LText -> P.Sem r ()
respondToCommand msg command = M.getConfig >>= \conf -> 
    let 
        secCommands cmd pwd = when (pwd == (M.password conf))  
                            (case cmd of
                                "ping" -> void $ tell msg ("pong" :: Text)
                                "goodbye" -> void $ tell msg ("goodbye" :: Text) >> stopBot)
        publicCommands cmd = case cmd of
                                "hello" -> void $ tell msg ("Hey there ;)" :: Text)
                                "heal" -> void $ tell msg ("HEAL NIVROC!!!111" :: Text)
                                "help" -> void $ tell msg ("Currently I have:\n - help command to see if I'm awake\n - today command to check for todays buffs" :: Text)
                                "today" -> do text <- P.input
                                              void $ tell msg ("Here's what I got today: \n" <> text)
                                _ -> void $ tell msg ("Hey, I don't know( Maybe you can teach me?" :: Text)                  
    in do
        let (cmd, pwd) = case words $ toText command of
                            (cmd : pwd : []) -> (cmd, pwd)
                            (cmd : []) -> (cmd, "")
                            _ -> ("", "")
        when (getID @Channel msg `elem` (M.controls conf) )
             (if E.null pwd then publicCommands cmd else secCommands cmd pwd)


retranslateOrPass :: (BotC r, P.Member M.GetConfig r, P.Member (P.Output Message) r) => 
                     Message -> Snowflake Channel -> P.Sem r ()
retranslateOrPass msg output = M.getConfig >>= \conf -> 
    let message = toText $ msg ^. #content
        muffinSource = M.toMuffinSource conf $ getID @Channel msg
    in when (M.checkConditions msg muffinSource)
       (do void $ invoke $ CreateMessage output (M.messageToSend muffinSource message) 
           when (containsRegex [R.reMI|[0-9]?[0-9][. :]?[0-9][0-9]|] msg) (void $ P.output msg)
       )