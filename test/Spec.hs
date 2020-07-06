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

import Test.HUnit
import qualified Calamity.Types.Model.Channel.Message       as M
import qualified Polysemy                                   as P
import qualified Polysemy.Reader                            as P
import qualified Polysemy.Error                             as P
import qualified Data.Vector.Unboxed as T
import Data.Time.Clock

import           Relude 
import           Conditions

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
import           Lib
import           Conditions

main :: IO ()
main = runTestTT tests >>= print
    
tests = TestList [TestLabel "test1" test1]

test1 = TestCase $ do
    time <- getCurrentTime
    result1 <- checkMessageContent $ ("rend pop":: LText)
    result2 <- checkMessageContent $ ("wtb WCB buff":: LText)
    result3 <- checkMessageContent $ ("whisper me for DMT":: LText)
    result4 <- checkMessageContent $ ("inv to whatever xD":: LText)
    assertEqual "rend pop" True result1
    assertEqual "wtb WCB buff" True result2
    assertEqual "whisper me for DMT" True result3
    assertEqual "inv to whatever xD" True result4

checkMessageContent :: LText -> IO Bool
checkMessageContent content = do
    time <- getCurrentTime
    P.runM . appConfigFrom $ do
        conf <- getConfig
        return $ passthrough (defMess time content) (posCond conf) (negCond conf)

defMess time content = M.Message{
    M.id = getID ("123"::Text),
    M.channelID = getID ("123"::Text),
    M.guildID = Nothing ,
    M.author = getID ("123"::Text), 
    M.content = content,
    M.timestamp = time, 
    M.editedTimestamp = Nothing,
    M.tts = False,
    M.mentionEveryone = False,
    M.mentions = T.empty,
    M.mentionRoles = T.empty,
    M.attachments = [],
    M.embeds = [],
    M.reactions = [],
    M.nonce = Nothing,
    M.pinned = False,
    M.webhookID = Nothing, 
    M.type_ = M.Default
} 

    