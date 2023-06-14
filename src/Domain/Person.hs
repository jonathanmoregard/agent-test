{-# LANGUAGE DataKinds #-}

module Domain.Person where

import Data.Finite (Finite)
import Data.Finite qualified as Finite
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.BigFive (BigFive)

data Person = Person
  { name :: Text,
    bigFive :: BigFive
  }
