{-# LANGUAGE OverloadedStrings #-}

module Program where

import Effect.Console (ConsoleE)
import Effect.Console qualified as ConsoleE
import Effect.LLM (LLMEffect)
import Effect.LLM qualified as LLME
import Effectful (Eff, (:>))

program :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
program = lostInTranslation

lostInTranslation :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
lostInTranslation = do
  let seed = "hey, how are you?"
  res <- LLME.completeText ("Translate the following English text to French: '" <> seed <> "'")
  ConsoleE.print $ "E -> F" <> res

  res2 <- LLME.completeText ("Translate the following English text to French: '" <> res <> "'")
  ConsoleE.print $ "F -> E" <> res2
