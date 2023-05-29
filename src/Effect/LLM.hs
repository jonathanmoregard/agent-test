{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.LLM (LLMEffect, completeText, runLLMEffect) where

import Control.Exception (IOException)
import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Client.TLS qualified as HTTPClient
import OpenAI.Client (OpenAIClient, TextCompletionCreate (tccrMaxTokens))
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources
  ( EngineId,
    TextCompletion (tcChoices),
    TextCompletionChoice (tccText),
    defaultTextCompletionCreate,
  )
import System.Environment (getEnv, lookupEnv)
import UnliftIO (catch)

data LLMEffect :: Effectful.Effect where
  CompleteText :: Text -> LLMEffect m Text

type instance DispatchOf LLMEffect = 'Dynamic

completeText :: LLMEffect :> es => Text -> Eff es Text
completeText prompt = send $ CompleteText prompt

runLLMEffect ::
  (IOE :> es, Error Text :> es) =>
  OpenAIClient ->
  EngineId ->
  Eff (LLMEffect : es) a ->
  Eff es a
runLLMEffect client engine = interpret $ \_ -> \case
  CompleteText prompt -> do
    result <- adapt $ OpenAI.completeText client engine $ (defaultTextCompletionCreate prompt) {tccrMaxTokens = Just 1000}
    case result of
      Left err -> throwError $ "Client error:" <> T.pack (show err)
      Right response -> do
        let choices = tcChoices response
        if V.null choices
          then throwError $ T.pack "No choices in response"
          else pure $ T.dropWhile (== '\n') $ tccText (V.head choices)
    where
      adapt :: (IOE :> es, Error Text :> es) => IO a -> Eff es a
      adapt m = liftIO m `catch` \(e :: IOException) -> throwError $ T.pack (show e)