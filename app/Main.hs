{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effect.Console qualified as ConsoleE
import Effect.LLM qualified as LLME
import Effectful qualified as IOE
import Effectful.Error.Static (CallStack, prettyCallStack, throwError)
import Effectful.Error.Static qualified as Error
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client (OpenAIClient)
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources (EngineId (EngineId))
import Program
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  configKvp <- fmap (second (T.replace "=" "") . T.breakOn "=") . T.lines <$> TIO.readFile "app.config" -- get config key value pairs
  IOE.runEff $ ConsoleE.runConsoleE $ do
    (result :: Either (CallStack, Text) ()) <- Error.runError $ do
      client <- case lookup "OpenAiSecret" configKvp of
        Nothing -> throwError $ T.pack "Could not find OPENAI_KEY in app.config."
        Just apiKey -> pure $ OpenAI.makeOpenAIClient apiKey manager 8
      LLME.runLLMEffect client Program.program
    case result of
      Right () -> pure ()
      Left (cs, err) -> ConsoleE.print $ "Error: " <> show err <> ", callstack: " <> prettyCallStack cs
