{-# LANGUAGE OverloadedStrings #-}

module Program where

import Control.Monad (foldM, foldM_)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Console (ConsoleE)
import Effect.Console qualified as ConsoleE
import Effect.LLM (LLMEffect)
import Effect.LLM qualified as LLME
import Effectful (Eff, (:>))

program :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
program = aConversation

newtype ConversationHistory
  = ConversationHistory [Quote]

addToHistory :: Quote -> ConversationHistory -> ConversationHistory
addToHistory q (ConversationHistory h) = ConversationHistory $ h ++ [q]

prettyHistory :: ConversationHistory -> Text
prettyHistory (ConversationHistory h) = Text.unlines $ prettyQuote <$> h

newtype Name
  = Name Text
  deriving (IsString)

data Quote = Quote
  { name :: Name,
    statement :: Text
  }

prettyQuote :: Quote -> Text
prettyQuote q = (coerce q.name) <> ": " <> q.statement

aConversation :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
aConversation = do
  let person1 = "Muhammed Lee"
  let person2 = "Torgny Segerstedt"
  ConsoleE.print "Muhammed Lee: I'm so mad right now"
  foldM_ answer (ConversationHistory []) $ take 20 $ cycle [person2, person1]

answer :: (ConsoleE :> es, LLMEffect :> es) => ConversationHistory -> Name -> Eff es ConversationHistory
answer history name = do
  reply <-
    LLME.completeText $
      Text.unlines
        [ "Here is the context:",
          "You are '" <> coerce name <> "'.",
          "You are standing talking to another person",
          "--------------------",
          "Muhammed Lee: I'm so mad right now",
          prettyHistory history,
          coerce name <> ": "
        ]
  let quote = Quote name reply
  ConsoleE.print $ prettyQuote quote
  pure $ addToHistory quote history