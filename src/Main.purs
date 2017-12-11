module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Data.Either (Either(Right, Left))
import Data.Foldable (for_, traverse_)
import Data.List.Lazy (replicateM)
import Data.List.NonEmpty (NonEmptyList)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, un)
import Data.String (null)
import Data.String.Regex (Regex, regex, test)
import Data.Validation.Semigroup (V, invalid, unV)

main :: forall e. Eff (console :: CONSOLE, randExp :: RANDEXP | e) Unit
main = do
  let validated = validatePerson { address: "nope", age: 35, name: "Chris" }
  unV (traverse_ printError) printPerson validated

type RawPerson
  = { address :: String
    , age :: Int
    , name :: String
    }

type Person
  = { address :: Address
    , age :: Age
    , name :: Name
    }

newtype Address
  = Address String

derive instance newtypeAddress :: Newtype Address _

newtype Age
  = Age Int

derive instance newtypeAge :: Newtype Age _

newtype Name
  = Name String

derive instance newtypeName :: Newtype Name _

data Error
  = DeveloperError String
  | BadAddress String RandExp
  | BlankName
  | TooOld Int
  | TooYoung Int

type Errors
  = NonEmptyList Error

printError :: forall e. Error -> Eff (console :: CONSOLE, randExp :: RANDEXP | e) Unit
printError = case _ of
  DeveloperError error -> log ("There was a problem with the code: " <> error)
  BadAddress address re -> do
    examples <- replicateM 3 (runEffFn1 gen re)
    log "Address was not the correct format"
    log "Given:"
    log $ "  " <> address
    log "Some examples of valid addresses include:"
    for_ examples \example -> do
      log $ "  " <> example
  BlankName -> log "Name cannot be blank"
  TooOld age -> log "Age is too old"
  TooYoung age -> log "Age is too young"

printPerson :: forall e. Person -> Eff (console :: CONSOLE | e) Unit
printPerson { address, age, name } =
  log
    $ un Name name
    <> " is "
    <> show (un Age age)
    <> " years old and lives at "
    <> un Address address

validateAddress :: String -> V Errors Address
validateAddress str = case regex """^[1-9]\d{0,3} [A-Z][a-zA-Z]{0,10} (Ave|Ln|St)$""" mempty of
  Left error -> invalid $ pure $ DeveloperError error
  Right re
    | test re str -> pure $ Address str
    | otherwise -> invalid $ pure $ BadAddress str $ randExp re

validateAge :: Int -> V Errors Age
validateAge n
  | n < 35 = invalid $ pure $ TooYoung n
  | 45 < n = invalid $ pure $ TooOld n
  | otherwise = pure $ Age n

validateName :: String -> V Errors Name
validateName str
  | not null str = pure $ Name str
  | otherwise = invalid $ pure BlankName

validatePerson :: RawPerson -> V Errors Person
validatePerson { address, age, name } =
  pure { address: _, age: _, name: _ }
    <*> validateAddress address
    <*> validateAge age
    <*> validateName name

foreign import data RANDEXP :: Effect

foreign import data RandExp :: Type

foreign import randExp :: Regex -> RandExp

foreign import gen :: forall e. EffFn1 (randExp :: RANDEXP | e) RandExp String
