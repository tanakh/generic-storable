{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
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
main = hspecX $ do
  describe "generic storable" $ do
    prop "instanciate a simple type" $ \i -> morallyDubiousIOProperty $ do
      idPokePeek $ SimpleType i
    prop "instanceate a type has multiple records" $ \i d -> morallyDubiousIOProperty $ do
      idPokePeek $ MultipleRecords i d
    prop "instanceate a type has multiple records with names" $ \i d -> morallyDubiousIOProperty $ do
      idPokePeek $ MultipleRecordsWithNames i d
    prop "instanceate a type has multiple constructor" $ do
      typ <- elements [0,1,2]
      v <- case typ ::Int of
        0 -> ConA <$> arbitrary
        1 -> ConB <$> arbitrary
        2 -> ConC <$> arbitrary <*> arbitrary
      morallyDubiousIOProperty $ do
        idPokePeek v
    prop "instanceate a type has multiple constructor" $ \i c d -> morallyDubiousIOProperty $ do
        idPokePeek $ TypeWithParameters (i :: Int) (c :: Char) (d :: Double)
