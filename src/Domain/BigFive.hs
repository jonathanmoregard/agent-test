{-# LANGUAGE DataKinds #-}

module Domain.BigFive where

import Data.Finite (Finite)
import Data.Text (Text)
import Data.Text qualified as Text

data BigFive = BigFive
  { openness :: Finite 10,
    extroversion :: Finite 10,
    conscientiousness :: Finite 10,
    neuroticism :: Finite 10,
    agreeableness :: Finite 10
  }

pretty :: BigFive -> Text
pretty bf =
  Text.unwords
    [ Text.unwords ["Openness to experience:", Text.pack $ show $ openness bf, "/10."],
      Text.unwords ["Extroversion:", Text.pack $ show $ extroversion bf, "/10."],
      Text.unwords ["Conscientiousness:", Text.pack $ show $ conscientiousness bf, "/10."],
      Text.unwords ["Neuroticism:", Text.pack $ show $ neuroticism bf, "/10."],
      Text.unwords ["Agreeableness:", Text.pack $ show $ adjustForChatGPTTraining $ agreeableness bf, "/10."]
    ]
  where
    adjustForChatGPTTraining :: Finite 10 -> Finite 10
    adjustForChatGPTTraining f = f `div` 2
