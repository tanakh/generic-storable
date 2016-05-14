{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

data SimpleType = SimpleType Int deriving (Eq, Show, Generic)

data MultipleRecords =
  MultipleRecords Int Double deriving (Eq, Show, Generic)

data MultipleRecordsWithNames =
  MultipleRecordsWithNames { a :: Int, b :: Double } deriving (Eq, Show, Generic)

data MultipleConstructors
  = ConA Int
  | ConB Double
  | ConC Bool Char
  deriving (Eq, Show, Generic)

data TypeWithParameters a b c =
  TypeWithParameters a b c
  deriving (Eq, Show, Generic)

idPokePeek :: (Generic a, GStorable (Rep a), Eq a) => a -> IO Bool
idPokePeek v = alloca $ \ptr -> do
  poke ptr (StorableWrapper v)
  StorableWrapper w <- peek ptr
  return $ v == w

main :: IO ()
main = hspec $ do
  describe "generic storable" $ do
    prop "instanciate a simple type" $ \i -> ioProperty $ do
      idPokePeek $ SimpleType i
    prop "instanciate a type with multiple records" $ \i d -> ioProperty $ do
      idPokePeek $ MultipleRecords i d
    prop "instanciate a type with multiple records with names" $ \i d -> ioProperty $ do
      idPokePeek $ MultipleRecordsWithNames i d
    prop "instanciate a type with multiple constructors" $ do
      typ <- elements [0,1,2]
      v <- case typ :: Int of
        0 -> ConA <$> arbitrary
        1 -> ConB <$> arbitrary
        2 -> ConC <$> arbitrary <*> arbitrary
      return $ ioProperty $ do
        idPokePeek v
    prop "instanciate a type with parameters" $ \i c d -> ioProperty $ do
        idPokePeek $ TypeWithParameters (i :: Int) (c :: Char) (d :: Double)
