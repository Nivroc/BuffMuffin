{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Model where

import qualified Polysemy           as P
import           Control.Lens
import           Calamity
import           Relude
import           Conditions
import           Text.RE.TDFA.Text  as R
import           Data.Default.Class
import           Data.Configurator.Types
import           Data.Configurator

data AppConfig = AppConfig {
    myID :: Snowflake User,
    mrally :: Snowflake Channel,
    mrhorde :: Snowflake Channel,
    mrdmt :: Snowflake Channel,
    mrheart :: Snowflake Channel,
    controls :: [Snowflake Channel],
    output :: Snowflake Channel,
    password :: Text
}

fromCfgFile :: Config -> IO AppConfig
fromCfgFile cfg = do
    myID <- (getID @User) <$> lookupDefault ("728488898669445150" :: Text) cfg "myid"
    mrally <- (getID @Channel) <$> lookupDefault ("731183786636869773" :: Text) cfg "channeldict.mrally"
    mrhorde <- (getID @Channel) <$> lookupDefault ("731183759705112697" :: Text) cfg "channeldict.mrhorde"
    mrdmt <- (getID @Channel) <$> lookupDefault ("731183831826169976" :: Text) cfg "channeldict.mrdmt"
    mrheart <- (getID @Channel) <$> lookupDefault ("731183813341872159" :: Text) cfg "channeldict.mrheart"
    controls <- (fmap . fmap $ getID @Channel) (lookupDefault ["725657927033028672":: Text] cfg "channeldict.controls")
    output <- (getID @Channel) <$> lookupDefault ("725657927033028672" :: Text) cfg "channeldict.output"
    password <- require cfg "password"
    return $ AppConfig myID mrally mrhorde mrdmt mrheart controls output password


data MuffinType = Control | Source |Output
data MuffinSource = MRAlliance | MRHorde | MRHeart | MRDireMaulTribute | Other (Snowflake Channel)

toMuffinSource :: AppConfig -> Snowflake Channel -> MuffinSource
toMuffinSource cfg cid
    | cid == mrally cfg = MRAlliance
    | cid == mrhorde cfg = MRHorde
    | cid == mrheart cfg = MRHeart
    | cid == mrdmt cfg = MRDireMaulTribute
    | otherwise = Other cid


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


freqPos = [containsRegex [R.reMI|[0-9]?[0-9][. :]?[0-9][0-9]|],
            containsRegex [R.reMI|in [0-9]?[0-9]|]]
freqNeg = [anyKeywords ["?", "when", "anyone", "not sure", "unsure", "plz ", "pls ", "please"],
            containsRegex [R.reMI|[0-9][0-9][0-9][0-9][0-9]|]] 

checkConditions :: (Conditionable a) => Message -> a -> Bool
checkConditions msg muff = passthrough msg (muffpass muff) (mufffail muff) 

regularContent :: Text -> Text -> CreateMessageOptions
regularContent prefix content = #content .~ (Just $ prefix <> content) $ def 

class Conditionable a where
    muffpass :: a -> [Condition] 
    muffpass _ = freqPos   
    mufffail :: a -> [Condition]
    mufffail _ = freqNeg                     
    
    messageToSend :: a -> Text -> CreateMessageOptions

instance Conditionable MuffinSource where
    muffpass MRAlliance = freqPos ++ [anyKeywords ["warning "]]
    muffpass MRHorde = freqPos
    muffpass MRHeart = freqPos ++ [anyKeywords ["heart", "zg "]]
    muffpass MRDireMaulTribute = freqPos ++ [anyKeywords ["whisp", "inv", "/w ", "open"]]
    muffpass (Other c) = [havanaRoomMention (getID $ ("609639269459427339" :: Text))]

    mufffail MRAlliance = freqNeg
    mufffail MRHorde = freqNeg ++ [anyKeywords ["summon", "ony", "nef"]]
    mufffail MRHeart = freqNeg
    mufffail MRDireMaulTribute = freqNeg ++ [anyKeywords ["warning "]]
    mufffail (Other c) = []

    messageToSend MRAlliance c = regularContent "MRAlliance: " c
    messageToSend MRHorde content = if anyKeywordsPT ["rend ", "wcb ", "warning"] content 
                                    then #allowedMentions .~ roles $ regularContent "<@&721396284061122560> MRHorde(rend): " content 
                                    else regularContent "MRHorde(rend): " content
                                    where roles = Just (set #roles [(getID $ ("721396284061122560" :: Text))] def)
    messageToSend MRHeart c = regularContent "MRHeart: " c
    messageToSend MRDireMaulTribute c = regularContent "MRDireMaulTribute: " c
    messageToSend (Other t) c = regularContent "Other server: " c
