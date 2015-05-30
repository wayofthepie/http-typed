{-# LANGUAGE
     DataKinds
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}

module QueryString where

import Control.Applicative hiding (empty)
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.ByteString.Lazy hiding (foldl, foldl1)
import Data.Proxy
import Data.Monoid
import GHC.TypeLits

{-
    Toying with well typed query strings for Docker's /images/create
    endpoint.
-}

-- | Parameters which can be set
data FromImage  = FromImage Builder
data FromSrc    = FromSrc Builder
data Registry   = Registry Builder
data Repo       = Repo Builder
data Tag        = Tag Builder


instance ToByteString FromImage where
    builder (FromImage b) = b

type family TypedKey (k :: *) :: Symbol where
    TypedKey FromImage = "fromImage"

data QueryKeyVal (k :: Symbol) a :: * where
    QueryKeyVal :: a -> QueryKeyVal (TypedKey a) a



class ToQueryString a where
    buildQueryString :: a -> ByteString


instance (k ~ TypedKey a, KnownSymbol k, ToByteString a) => ToQueryString (QueryKeyVal k a) where
        buildQueryString (QueryKeyVal a) =
            (BC.pack $ symbolVal(Proxy :: Proxy k)) <> "=" <> toByteString a


instance ToQueryString a => ToQueryString [a] where
    buildQueryString (x:xs) =
        foldl (\bs val -> bs <> "&" <> buildQueryString val)
              (buildQueryString x)
              xs


instance (KnownSymbol k, Show a) => Show (QueryKeyVal k a) where
    show (QueryKeyVal a) = "QueryKeyVal "
        <> symbolVal (Proxy :: Proxy k)
        <> " "
        <> show a


