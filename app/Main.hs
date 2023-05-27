{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client
import OpenAI.Resources
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Vector as V
import Data.Either (Either, either)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  maybeApiKey <- lookupEnv "OPENAI_KEY"
  case maybeApiKey of
    Nothing -> putStrLn "Could not find OPENAI_KEY in environment variables."
    Just apiKey -> do
      let client = makeOpenAIClient (T.pack apiKey) manager 4
      let engineId = EngineId "text-davinci-002"  -- EngineId may vary, check OpenAI API docs
      let prompt = defaultTextCompletionCreate "Translate the following English text to French: '{text}'"
      result <- completeText client engineId prompt
      case result of
        Left err -> print err  -- Print the error if the request fails
        Right response -> do
          let choices = tcChoices response
          if V.null choices
            then putStrLn "No choices in the response."
            else do
              let firstChoice = V.head choices
              let outputText = tccText firstChoice
              print outputText
