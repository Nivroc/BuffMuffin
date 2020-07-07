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

--main = print "tests passed"

main :: IO ()
main = runTestTT tests >>= print
   
tests = TestList [TestLabel "Positive" test1, TestLabel "Negative" test2]

test1 = TestCase $ do
    time <- getCurrentTime
    result1 <- checkMessageContent  ("rend pop":: LText)
    result2 <- checkMessageContent  ("wtb WCB buff":: LText)
    result3 <- checkMessageContent  ("whisper me for DMT":: LText)
    result4 <- checkMessageContent  ("inv to whatever xD":: LText)
    result5 <- checkMessageContent  ("ony buff at 19.20":: LText)
    result6 <- checkMessageContent  ("Next on the lineup is :\n\n19:10 - Onyxia head by @<Quantum> Tsusaka\n\n19:30 - <Chaos Knights> Planned Nefarian head":: LText)
    assertEqual "rend pop" True result1
    assertEqual "wtb WCB buff" True result2
    assertEqual "whisper me for DMT" True result3
    assertEqual "inv to whatever xD" True result4
    assertEqual "ony buff at 19.20" True result5
    assertEqual "ony buff at 19.20" True result6


test2 = TestCase $ do
    time <- getCurrentTime
    result1 <- checkMessageContent  ("Any pops today":: LText)
    result2 <- checkMessageContent  ("popping now?":: LText)
    result3 <- checkMessageContent  ("not sure about any buffs":: LText)
    result4 <- checkMessageContent  ("whens heart popping":: LText)
    result5 <- checkMessageContent  ("ony buff at 19.20?":: LText)
    result6 <- checkMessageContent  ("find someone with a head :4head:":: LText)
    assertEqual "Any pops today" False result1
    assertEqual "popping now?" False result2
    assertEqual "not sure about any buffs" False result3
    assertEqual "whens heart popping" False result4
    assertEqual "ony buff at 19.20?" False result5
    assertEqual "find someone with a head :4head:" False result6

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
    M.mentionChannels = Nothing,
    M.attachments = [],
    M.embeds = [],
    M.reactions = [],
    M.nonce = Nothing,
    M.pinned = False,
    M.webhookID = Nothing, 
    M.type_ = M.Default
} 

    