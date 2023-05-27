{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effect.Console (ConsoleE)
import Effect.Console qualified as ConsoleE
import Effect.LLM (LLMEffect, Prompt (..), Response (..))
import Effect.LLM qualified as LLME
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful qualified as IOE
import Effectful.Error.Static (CallStack, Error, prettyCallStack, throwError)
import Effectful.Error.Static qualified as Error
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client (OpenAIClient)
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources (EngineId (EngineId))
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  configKvp <- fmap (second (T.replace "=" "") . T.breakOn "=") . T.lines <$> TIO.readFile "app.config" -- get config key value pairs
  let engineId = EngineId "text-davinci-002" -- EngineId may vary, check OpenAI API docs
  IOE.runEff $ ConsoleE.runConsoleE $ do
    (result :: Either (CallStack, Text) ()) <- Error.runError $ do
      client <- case lookup "OpenAiSecret" configKvp of
        Nothing -> throwError $ T.pack "Could not find OPENAI_KEY in app.config."
        Just apiKey -> pure $ OpenAI.makeOpenAIClient apiKey manager 4
      LLME.runLLMEffect client engineId program
    case result of
      Right () -> pure ()
      Left (cs, err) -> liftIO $ print $ "Error: " <> show err <> ", callstack: " <> prettyCallStack cs

program :: (ConsoleE :> es, LLMEffect :> es) => Eff es ()
program = do
  res <- LLME.completeText (Prompt "Translate the following English text to French: 'Hello, how are you?'")
  ConsoleE.print res
