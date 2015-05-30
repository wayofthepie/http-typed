{-# LANGUAGE
     DataKinds
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}

module QueryString where

import Control.Applicative hiding (empty)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.ByteString.Lazy hiding (foldl, foldl1)
import Data.Proxy
import Data.Monoid
import GHC.TypeLits


data FromImage = FromImage ByteString deriving Show

class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString FromImage where
    toByteString (FromImage bs) = bs

type family TypedKey (k :: *) :: Symbol where
    TypedKey FromImage = "fromImage"

data QueryKeyVal (k :: Symbol) a :: * where
    QueryKeyVal :: a -> QueryKeyVal (TypedKey a) a

class QueryString a where
    buildQueryString :: a -> ByteString

instance (KnownSymbol k, Show a) => Show (QueryKeyVal k a) where
    show (QueryKeyVal a) = "QueryKeyVal "
        <> symbolVal (Proxy :: Proxy k)
        <> " "
        <> show a


instance (k ~ TypedKey a, KnownSymbol k, ToByteString a) => QueryString (QueryKeyVal k a) where
        buildQueryString (QueryKeyVal a) =
            (BC.pack $ symbolVal(Proxy :: Proxy k)) <> "=" <> toByteString a

instance QueryString a => QueryString [a] where
    buildQueryString (x:xs) =
        foldl (\bs val -> bs <>  "&" <> buildQueryString val)
              (buildQueryString x)
              xs

