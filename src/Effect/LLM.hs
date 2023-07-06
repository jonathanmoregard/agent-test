{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.LLM (LLMEffect, completeText, generateEmbedding, runLLMEffect) where

import Control.Exception (IOException)
import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.EmbeddingVector (EmbeddingVector, embeddingVector)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Client.TLS qualified as HTTPClient
import OpenAI.Client (CompletionCreate (ccrMaxTokens), CompletionResponse, EmbeddingCreate (..), OpenAIClient, TextCompletionCreate (tccrMaxTokens))
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources
  ( EngineId,
    TextCompletion (tcChoices),
    TextCompletionChoice (tccText),
  )
import System.Environment (getEnv, lookupEnv)
import UnliftIO (catch)

data LLMEffect :: Effectful.Effect where
  CompleteText :: Text -> LLMEffect m Text
  GenerateEmbedding :: Text -> LLMEffect m EmbeddingVector

type instance DispatchOf LLMEffect = 'Dynamic

completeText :: LLMEffect :> es => Text -> Eff es Text
completeText prompt = send $ CompleteText prompt

generateEmbedding :: LLMEffect :> es => Text -> Eff es EmbeddingVector
generateEmbedding input = send $ GenerateEmbedding input

runLLMEffect ::
  (IOE :> es, Error Text :> es) =>
  OpenAIClient ->
  Eff (LLMEffect : es) a ->
  Eff es a
runLLMEffect client = interpret $ \_ -> \case
  CompleteText prompt -> do
    result <- adapt $ OpenAI.completeText client (OpenAI.defaultCompletionCreate (OpenAI.ModelId "text-davinci-003") prompt) {ccrMaxTokens = Just 1000}
    case result of
      Left err -> throwError $ "Client error:" <> T.pack (show err)
      Right response -> do
        let choices = OpenAI.crChoices response
        if null choices
          then throwError $ T.pack "No choices in response"
          else pure $ T.dropWhile (== '\n') $ OpenAI.cchText (head choices)
  GenerateEmbedding input -> do
    let embeddingCreate = EmbeddingCreate (OpenAI.ModelId "text-embedding-ada-002") input Nothing
    result <- adapt $ OpenAI.createEmbedding client embeddingCreate
    case result of
      Left err -> throwError $ "Client error:" <> T.pack (show err)
      Right response -> pure . embeddingVector . OpenAI.embdEmbedding . head . OpenAI.embrData $ response
  where
    adapt :: (IOE :> es, Error Text :> es) => IO a -> Eff es a
    adapt m = liftIO m `catch` \(e :: IOException) -> throwError $ T.pack (show e)
