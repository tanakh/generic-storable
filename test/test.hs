{-# LANGUAGE DeriveGeneric, FlexibleContexts, ScopedTypeVariables #-}
import Control.Applicative
import Data.Foldable
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM

data SimpleType = SimpleType Int deriving (Eq, Show, Generic)

instance Arbitrary SimpleType where
  arbitrary = SimpleType <$> arbitrary

data MultipleRecords =
  MultipleRecords Int Double deriving (Eq, Show, Generic)

instance Arbitrary MultipleRecords where
  arbitrary = MultipleRecords <$> arbitrary <*> arbitrary

data MultipleRecordsWithNames =
  MultipleRecordsWithNames { a :: Int, b :: Double } deriving (Eq, Show, Generic)

instance Arbitrary MultipleRecordsWithNames where
  arbitrary = MultipleRecordsWithNames <$> arbitrary <*> arbitrary

data MultipleConstructors
  = ConA Int
  | ConB Double
  | ConC Bool Char
  deriving (Eq, Show, Generic)

instance Arbitrary MultipleConstructors where
  arbitrary = do
    typ <- elements [0,1,2]
    case typ :: Int of
      0 -> ConA <$> arbitrary
      1 -> ConB <$> arbitrary
      2 -> ConC <$> arbitrary <*> arbitrary

data TypeWithParameters a b c =
  TypeWithParameters a b c
  deriving (Eq, Show, Generic)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TypeWithParameters a b c) where
  arbitrary = TypeWithParameters <$> arbitrary <*> arbitrary <*> arbitrary

idPokePeek :: (Generic a, GStorable (Rep a), Eq a) => a -> IO Bool
idPokePeek v = alloca $ \ptr -> do
  poke ptr (StorableWrapper v)
  StorableWrapper w <- peek ptr
  return $ v == w

idPokePeekVec :: forall a. (Generic a, GStorable (Rep a), Eq a) => [a] -> IO Bool
idPokePeekVec l = do
    v <- VSM.new (length l)
    forM_ (l `zip` [0..]) $ \(x, i) -> 
        VSM.write v i (StorableWrapper x)
    v' <- VS.freeze v
    let l' = map unStorableWrapper $ VS.toList v'
    return $ l == l'

main :: IO ()
main = hspec $ do
  describe "generic storable" $ do
    prop "instanciate a simple type" $ \i -> 
      ioProperty $ idPokePeek (i :: SimpleType)
    prop "instanciate a type with multiple fields" $ \i -> 
      ioProperty $ idPokePeek (i :: MultipleRecords)
    prop "instanciate a type with multiple fields with names" $ \i -> 
      ioProperty $ idPokePeek (i :: MultipleRecordsWithNames)
    prop "instanciate a type with multiple constructors" $ \i ->
      ioProperty $ idPokePeek (i :: MultipleConstructors)
    prop "instanciate a type with parameters" $ \i -> 
      ioProperty $ idPokePeek (i :: TypeWithParameters Int Char Double)

  describe "storable vectors" $ do
    prop "vector of simple type" $ \xs -> 
      ioProperty $ idPokePeekVec (xs :: [SimpleType])
    prop "vector of type with multiple fields" $ \xs -> 
      ioProperty $ idPokePeekVec (xs :: [MultipleRecords])
    prop "vector of type with multiple fields with names" $ \xs -> 
      ioProperty $ idPokePeekVec (xs :: [MultipleRecordsWithNames])
    prop "vector of type with multiple constructors" $ \xs -> 
      ioProperty $ idPokePeekVec (xs :: [MultipleConstructors])
    prop "vector of type with parameters" $ \xs -> 
      ioProperty $ idPokePeekVec (xs :: [TypeWithParameters Int Char Double])

