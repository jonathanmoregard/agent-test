{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client
import OpenAI.Resources
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  configKvp <- fmap (second (T.replace "=" "") . T.breakOn "=") . T.lines <$> TIO.readFile "app.config" -- get config key value pairs
  case lookup "OpenAiSecret" configKvp of
    Nothing -> putStrLn "Could not find OPENAI_KEY in app.config."
    Just apiKey -> do
      let client = makeOpenAIClient apiKey manager 4
      let engineId = EngineId "text-davinci-002" -- EngineId may vary, check OpenAI API docs
      let prompt = defaultTextCompletionCreate "Translate the following English text to French: 'Hello, how are you?'"
      result <- completeText client engineId prompt
      case result of
        Left err -> print err -- Print the error if the request fails
        Right response -> do
          let choices = tcChoices response
          if V.null choices
            then putStrLn "No choices in the response."
            else do
              let firstChoice = V.head choices
              let outputText = tccText firstChoice
              print outputText
