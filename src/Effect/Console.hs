{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Console where

import Control.Exception (IOException)
import Data.Either (Either, either)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import System.IO (hSetEncoding, stdout, utf8)

data ConsoleE :: Effectful.Effect where
  Print :: Show a => a -> ConsoleE m ()

type instance DispatchOf ConsoleE = 'Dynamic

print :: (ConsoleE :> es, Show a) => a -> Eff es ()
print prompt = send $ Print prompt

runConsoleE ::
  (IOE :> es) =>
  Eff (ConsoleE : es) a ->
  Eff es a
runConsoleE = interpret $ \_ -> \case
  Print a -> liftIO $ do
    hSetEncoding stdout utf8
    Prelude.print a