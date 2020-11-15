{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Module with newtypes suitable to usage with @DerivingVia@ or standalone.
--
-- The newtypes are named after packages they wrap.
module Database.PostgreSQL.Simple.Newtypes (
    Aeson (..), getAeson,
) where

import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)

import qualified Data.Aeson as Aeson

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

-- | A newtype wrapper with 'ToField' and 'FromField' instances
-- based on 'Aeson.ToJSON' and 'Aeson.FromJSON' type classes from @aeson@.
--
-- Example using @DerivingVia@:
--
-- @
-- data Foo = Foo Int String
--   deriving stock (Eq, Show, Generic)            -- GHC built int
--   deriving anyclass ('Aeson.FromJSON', 'Aeson.ToJSON')          -- Derived using GHC Generics
--   deriving ('ToField', 'FromField') via 'Aeson' Foo   -- DerivingVia
-- @
--
-- Example using 'Aeson' newtype directly, for more ad-hoc queries
--
-- @
-- execute conn "INSERT INTO tbl (fld) VALUES (?)" (Only ('Aeson' x))
-- @
--  
--  @since 0.6.3
newtype Aeson a = Aeson a
  deriving (Eq, Show, Read, Typeable, Functor)

getAeson :: Aeson a -> a
getAeson (Aeson a) = a

instance Aeson.ToJSON a => ToField (Aeson a) where
    toField = toField . Aeson.encode . getAeson

instance (Aeson.FromJSON a, Typeable a) => FromField (Aeson a) where
    fromField f bs = fmap Aeson (fromJSONField f bs)
