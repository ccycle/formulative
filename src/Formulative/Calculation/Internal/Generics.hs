module Formulative.Calculation.Internal.Generics where

import Control.Applicative (Alternative (empty), (<|>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Csv (DefaultOrdered, Field, FromField (parseField), Name, NamedRecord, Options (fieldLabelModifier), Parser, Record, ToField (toField), (.:), (.=))
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics
import GHC.Natural

-- Reference:
-- https://www.stackbuilders.com/blog/generics/

class CountFields a where
    -- | Return number of constuctor fields for a value.
    countFields :: a -> Natural
    default countFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
    countFields = defaultCountFields

class CountFields1 f where
    countFields1 :: f p -> Natural

defaultCountFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
defaultCountFields = countFields1 . from

instance CountFields1 V1 where
    countFields1 _ = 0

instance CountFields1 U1 where
    countFields1 _ = 0

instance CountFields1 (K1 i c) where
    countFields1 _ = 1

instance CountFields1 f => CountFields1 (M1 i c f) where
    countFields1 (M1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :+: b) where
    countFields1 (L1 x) = countFields1 x
    countFields1 (R1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :*: b) where
    countFields1 (a :*: b) = countFields1 a + countFields1 b

----------------------------------------------------------------
-- from cassava
----------------------------------------------------------------

class GFromRecord f where
    gparseRecord :: Options -> Record -> Parser (f p)

instance GFromRecordSum f Record => GFromRecord (M1 i n f) where
    gparseRecord opts v =
        case IM.lookup n (gparseRecordSum opts) of
            Nothing -> lengthMismatch n v
            Just p -> M1 <$> p v
      where
        n = V.length v

lengthMismatch :: Int -> Record -> Parser (M1 i n f p)
lengthMismatch = undefined

class GFromNamedRecord f where
    gparseNamedRecord :: Options -> NamedRecord -> Parser (f p)

instance GFromRecordSum f NamedRecord => GFromNamedRecord (M1 i n f) where
    gparseNamedRecord opts v =
        foldr (\f p -> p <|> M1 <$> f v) empty (IM.elems (gparseRecordSum opts))

class GFromRecordSum f r where
    gparseRecordSum :: Options -> IM.IntMap (r -> Parser (f p))

instance (GFromRecordSum a r, GFromRecordSum b r) => GFromRecordSum (a :+: b) r where
    gparseRecordSum opts =
        IM.unionWith
            (\a b r -> a r <|> b r)
            (fmap (L1 <$>) <$> gparseRecordSum opts)
            (fmap (R1 <$>) <$> gparseRecordSum opts)

instance GFromRecordProd f r => GFromRecordSum (M1 i n f) r where
    gparseRecordSum opts = IM.singleton n (fmap (M1 <$>) f)
      where
        (n, f) = gparseRecordProd opts 0

class GFromRecordProd f r where
    gparseRecordProd :: Options -> Int -> (Int, r -> Parser (f p))

instance GFromRecordProd U1 r where
    gparseRecordProd _ n = (n, const (pure U1))

instance (GFromRecordProd a r, GFromRecordProd b r) => GFromRecordProd (a :*: b) r where
    gparseRecordProd opts n0 = (n2, f)
      where
        f r = (:*:) <$> fa r <*> fb r
        (n1, fa) = gparseRecordProd opts n0
        (n2, fb) = gparseRecordProd opts n1

instance GFromRecordProd f Record => GFromRecordProd (M1 i n f) Record where
    gparseRecordProd opts n = fmap (M1 <$>) <$> gparseRecordProd opts n

instance FromField a => GFromRecordProd (K1 i a) Record where
    gparseRecordProd _ n = (n + 1, \v -> K1 <$> parseField (V.unsafeIndex v n))

data Proxy1 s (f :: * -> *) a = Proxy1

instance (FromField a, Selector s) => GFromRecordProd (M1 S s (K1 i a)) NamedRecord where
    gparseRecordProd opts n = (n + 1, \v -> (M1 . K1) <$> v .: name)
      where
        name = T.encodeUtf8 (T.pack (fieldLabelModifier opts (selName (Proxy1 :: Proxy1 s f a))))

class GToRecord a f where
    gtoRecord :: Options -> a p -> [f]

instance GToRecord U1 f where
    gtoRecord _ U1 = []

instance (GToRecord a f, GToRecord b f) => GToRecord (a :*: b) f where
    gtoRecord opts (a :*: b) = gtoRecord opts a ++ gtoRecord opts b

instance (GToRecord a f, GToRecord b f) => GToRecord (a :+: b) f where
    gtoRecord opts (L1 a) = gtoRecord opts a
    gtoRecord opts (R1 b) = gtoRecord opts b

instance GToRecord a f => GToRecord (M1 D c a) f where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance GToRecord a f => GToRecord (M1 C c a) f where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance GToRecord a Field => GToRecord (M1 S c a) Field where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance ToField a => GToRecord (K1 i a) Field where
    gtoRecord _ (K1 a) = [toField a]

instance (ToField a, Selector s) => GToRecord (M1 S s (K1 i a)) (B.ByteString, B.ByteString) where
    gtoRecord opts m@(M1 (K1 a)) = [name .= toField a]
      where
        name = T.encodeUtf8 (T.pack (fieldLabelModifier opts (selName m)))

-- We statically fail on sum types and product types without selectors
-- (field names).

class GToNamedRecordHeader a where
    gtoNamedRecordHeader :: Options -> a p -> [Name]

instance GToNamedRecordHeader U1 where
    gtoNamedRecordHeader _ _ = []

instance
    (GToNamedRecordHeader a, GToNamedRecordHeader b) =>
    GToNamedRecordHeader (a :*: b)
    where
    gtoNamedRecordHeader opts _ =
        gtoNamedRecordHeader opts (undefined :: a p)
            ++ gtoNamedRecordHeader opts (undefined :: b p)

instance GToNamedRecordHeader a => GToNamedRecordHeader (M1 D c a) where
    gtoNamedRecordHeader opts _ = gtoNamedRecordHeader opts (undefined :: a p)

instance GToNamedRecordHeader a => GToNamedRecordHeader (M1 C c a) where
    gtoNamedRecordHeader opts _ = gtoNamedRecordHeader opts (undefined :: a p)

{- | Instance to ensure that you cannot derive DefaultOrdered for
 constructors without selectors.
 instance DefaultOrdered (M1 S NoSelector a ()) => GToNamedRecordHeader (M1 S NoSelector a)
   where
     gtoNamedRecordHeader _ _ =
         error "You cannot derive DefaultOrdered for constructors without selectors."
-}
instance Selector s => GToNamedRecordHeader (M1 S s a) where
    gtoNamedRecordHeader opts m
        | null name = error "Cannot derive DefaultOrdered for constructors without selectors"
        | otherwise = [B8.pack (fieldLabelModifier opts (selName m))]
      where
        name = selName m