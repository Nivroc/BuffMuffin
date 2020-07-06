{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TupleSections #-}

module Conditions where


import           Relude 
import           Calamity
import qualified Data.Text          as E
import           Control.Lens
import           Text.RE.TDFA.Text  as R
import           Control.Arrow
import           Control.Conditional (condDefault)


type Condition = Message -> Bool

passthrough :: Message -> [Condition] -> [Condition] -> Bool
passthrough msg posList negList = condDefault False targetList
    where targetList = ((\f -> (f msg, False)) <$> negList) ++ ((\f -> (f msg, True)) <$> posList)

anyKeywords :: [Text] -> Condition
anyKeywords words msg = getAny $ mconcat $ Any . flip E.isInfixOf (E.toUpper content) . E.toUpper <$> words
    where content = toText $ msg ^. #content

noneKeywords = (not .) . anyKeywords

fromChannel :: Snowflake Channel -> Condition
fromChannel chan msg = chan == msg ^. #channelID 

notFromChannel = (not .) . fromChannel

fromGuild :: Snowflake Guild -> Condition
fromGuild guild msg = maybe False (guild ==) (msg ^. #guildID) 

notFromGuild = (not .) . fromChannel

containsRegex :: R.RE -> Condition
containsRegex r msg = matched $ toText (msg ^. #content) ?=~ r

uncontainsRegex = (not .) . containsRegex

matchesRegex :: R.RE -> Condition
matchesRegex r msg = matched $ toText (msg ^. #content) ?=~ r

-- custom conditions

hordeWithoutNefOny hordechan = uncurry (&&) . (fromChannel hordechan &&& anyKeywords ["nef", "ony"])

try = (, True) <$> [1,2,3]