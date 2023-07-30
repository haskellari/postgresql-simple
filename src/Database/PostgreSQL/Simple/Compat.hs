{-# LANGUAGE CPP #-}
-- | This is a module of its own, partly because it uses the CPP extension,
-- which doesn't play well with backslash-broken string literals.
module Database.PostgreSQL.Simple.Compat
    ( mask
    , (<>)
    , unsafeDupablePerformIO
    , toByteString
    , scientificBuilder
    , toPico
    , fromPico
    ) where

import qualified Control.Exception as E
import Data.ByteString         (ByteString)
import Data.ByteString.Lazy    (toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Fixed (Pico)
import Data.Fixed (Fixed(MkFixed))

-- | Like 'E.mask', but backported to base before version 4.3.0.
--
-- Note that the restore callback is monomorphic, unlike in 'E.mask'.  This
-- could be fixed by changing the type signature, but it would require us to
-- enable the RankNTypes extension (since 'E.mask' has a rank-3 type).  The
-- 'withTransactionMode' function calls the restore callback only once, so we
-- don't need that polymorphism.
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask io = E.mask $ \restore -> io restore
{-# INLINE mask #-}

toByteString :: Builder -> ByteString
toByteString x = toStrict (toLazyByteString x)

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
