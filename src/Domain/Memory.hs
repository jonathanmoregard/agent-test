{-# LANGUAGE DataKinds #-}

module Domain.Memory where

import Data.Bifunctor (bimap)
import Data.Finite (Finite)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as TR
import Data.Time.Clock (UTCTime)
import Domain.BigFive qualified as BigFive
import Domain.Person (Person (..))
import Effect.LLM (LLMEffect)
import Effect.LLM qualified as LLM
import Effect.Time (TimeEffect)
import Effect.Time qualified as Time
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Extra.Effectful.Error.Static qualified as EError
import Text.Read qualified

data Memory = Memory
  { importance :: Finite 10,
    creationTime :: UTCTime,
    lastAccessedTime :: UTCTime,
    description :: Text
  }

makeMemory :: (LLMEffect :> es, Error Text :> es, TimeEffect :> es) => Person -> Text -> Eff es Memory
makeMemory person description = do
  now <- Time.now
  importance <- getImportance person description
  pure $
    Memory
      { importance = importance,
        creationTime = now,
        lastAccessedTime = now,
        description = description
      }

getImportance :: (LLMEffect :> es, Error Text :> es) => Person -> Text -> Eff es (Finite 10)
getImportance person description =
  EError.runEitherEff
    . fmap toFinite
    . LLM.completeText
    $ Text.unlines
      [ "On the scale of 1 to 10, where 1 is purely mundane (e.g., brushing teeth, making bed) and 10 is extremely poignant (e.g., a breakup, college acceptance),",
        "rate the likely poignancy of the following piece of memory",
        "Memories that are likely to happen every day are not very poignant, make sure you reduce the poignancy score for common occurrences.",
        "Big five psychological traits of the person: " <> BigFive.pretty person.bigFive,
        "Memory description: " <> description,
        "Note! Return only a number, no other text, your response will be parsed by a computer system"
      ]
  where
    toFinite :: Text -> Either Text (Finite 10)
    toFinite = bimap Text.pack fst . TR.decimal
