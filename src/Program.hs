{-# LANGUAGE OverloadedStrings #-}

module Program where

import Effect.Console (ConsoleE)
import Effect.Console qualified as ConsoleE
import Effect.LLM (LLMEffect, Prompt (..), Response (..))
import Effect.LLM qualified as LLME
import Effectful (Eff, (:>))

program :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
program = do
  res <- LLME.completeText (Prompt "Translate the following English text to French: 'Hello, how are you?'")
  ConsoleE.print res
