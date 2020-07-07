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

import           Calamity
import           Calamity.Metrics.Noop
import           Calamity.Cache.InMemory
import           Calamity.Commands
import qualified Calamity.Commands.Context                  as CommandContext
import           Calamity.Types.Model.User
import           Calamity.HTTP.Channel                      as C

import qualified Polysemy                                   as P
import qualified Polysemy.Reader                            as P
import qualified Polysemy.Output                            as P
import qualified Polysemy.Error                             as P
import qualified Polysemy.Input                             as P

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
import           Data.Time.Clock

import           Text.RE.TDFA.Text                          as R

data AppConfig = AppConfig {
    myID :: Snowflake User,
    sources :: [Snowflake Channel],
    controls :: [Snowflake Channel],
    output :: Snowflake Channel,
    posCond :: [Condition],
    negCond :: [Condition],
    password :: Text
}

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


appConfigFrom :: P.Sem (GetConfig : r) a -> P.Sem r a
appConfigFrom = P.interpret \case GetConfig -> pure cfg 
    where cfg = AppConfig {
        myID = getID ("725657610854072370"::Text),
        sources = getID <$> ["668419602337759253" :: Text, "702560043022811136", "668419781241864192", "700806643373441064"],
        controls = getID <$> ["725657927033028672" :: Text, "711192812682739802"],
        output = getID ("711192812682739802"::Text),
        --myID = getID ("728488898669445150"::Text),
        --sources = getID <$> ["725657927033028672" :: Text, "725657952400441374"],
        --controls = getID <$> ["725657927033028672" :: Text],
        --output = getID ("725657927033028672"::Text),
        posCond = [
            anyKeywords ["rend ", "wcb ", "whisper", "inv", "warning", "/w "],
            containsRegex [R.reMI|[0-9]?[0-9][. :]?[0-9][0-9]|],
            containsRegex [R.reMI|in [0-9]?[0-9]|],
            havanaRoomMention (getID $ ("609639269459427339" :: Text))
            --havanaRoomMention (getID $ ("725657927033028669" :: Text))

        ],
        negCond = [
            anyKeywords ["?", "when", "anyone", "not sure", "unsure", "plz ", "pls ", "please"],
         --   containsRegex [R.reMI|is .*|],
            hordeWithoutNefOny (getID $ ("668419781241864192" :: Text))
            --hordeWithoutNefOny (getID $ ("725657952400441374" :: Text))
        ],
        password = "havana123"
    }
    


data GetConfig m a where
    GetConfig :: GetConfig m AppConfig

P.makeSem ''GetConfig

coerceSF txt = case reads (toString txt) :: [(Word64,String)] of [(flake, "")] -> Snowflake flake  
instance HasID Channel Text where
    getID = coerceSF
instance HasID User Text where
    getID = coerceSF 
instance HasID Message Text where
    getID = coerceSF
instance HasID Guild Text where
    getID = coerceSF
instance HasID Role Text where
    getID = coerceSF

tellToId :: forall msg r t. (BotC r) => 
    Snowflake Channel -> Text -> Maybe C.AllowedMentions -> P.Sem r (Either RestError Message)
tellToId cid msg roles = P.runError $ do
  let messopts = #content .~ (Just msg) $ def 
  let withroles = #allowedMentions .~ roles $ messopts
  r <- invoke $ CreateMessage (prnt cid) messopts
  P.fromEither r
  where prnt flake = (show $ fromSnowflake $ flake) :: Text


someFunc :: IO ()
someFunc = void . P.runFinal . P.embedToFinal . runCacheInMemory . runMetricsNoop . 
            Lib.outToFile "/tmp/logs/" . Lib.inputFromFile "/tmp/logs/" . useConstantPrefix "muffin " . appConfigFrom $ 
    runBotIO (UserToken "NzI1NjU3NjEwODU0MDcyMzcw.XwCB5Q.aHBzDdQ_Uvw-9EdgIzI-DmhQsVI") $ 
    --runBotIO (UserToken "NzI4NDg4ODk4NjY5NDQ1MTUw.XwNO9A.FGcn0HXFf1T2JqN15gI5J7QbjRo")
    do  conf <- getConfig
        react @'MessageCreateEvt $ \msg -> 
            do parsePrefix msg >>= \case
                Just (prefix, cmd) -> respondToCommand msg cmd
                Nothing -> do 
                    print $ "Not a command: " <> show msg
                    when (getID @User msg /= myID conf) (void $ retranslateOrPass msg (output conf) (sources conf))
    

respondToCommand :: (BotC r, Tellable msg, P.Member GetConfig r, P.Member (P.Input Text) r, HasID Channel msg) => 
                    msg -> LText -> P.Sem r ()
respondToCommand msg command = getConfig >>= \conf -> 
    let 
        secCommands cmd pwd = when (pwd == (password conf))  
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
        when (getID @Channel msg `elem` (controls conf) )
             (if (E.null pwd) then publicCommands cmd else secCommands cmd pwd)

retranslateOrPass :: (BotC r, P.Member GetConfig r, P.Member (P.Output Message) r) => 
                     Message -> Snowflake Channel -> [Snowflake Channel] -> P.Sem r ()
retranslateOrPass msg output sources = getConfig >>= \conf -> 
    let retranslate m = passthrough m (posCond conf) (negCond conf)
        message = toText $ msg ^. #content
        correctSource = getID @Channel msg `elem` sources
    in when (correctSource && retranslate msg )
        (do 
            if ((fromChannel (getID $ ("668419781241864192" :: Text)) msg) && anyKeywords ["rend"] msg)
            --if (fromChannel (getID $ ("725657952400441374" :: Text)) msg)
            then void $ tellToId output ("<@&721396284061122560>Possible Rend Alert: " <> message) 
                    (Just (set #roles [(getID $ ("721396284061122560" :: Text))] def))
            else void $ tellToId output ("Alert: " <> message) Nothing
            when (containsRegex [R.reMI|[0-9]?[0-9][. :]?[0-9][0-9]|] msg)
                 (void $ P.output msg)
        )

