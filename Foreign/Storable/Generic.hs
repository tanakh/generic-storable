{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Foreign.Storable.Generic (
  -- * Generic Storable class
  GStorable(..),
  
  -- * Default functions
  sizeOfDefault,
  alignmentDefault,
  peekDefault,
  peekByteOffDefault,
  peekElemOffDefault,
  pokeDefault,
  pokeByteOffDefault,
  pokeElemOffDefault,

  -- * Wrapper
  StorableWrapper(..),
  ) where

import Control.Monad
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

-- | Generic Storable class
class GStorable f where
  gsizeOf :: f a -> Int
  
  galignment :: f a -> Int
  galignment = gsizeOf
  {-# INLINABLE galignment #-}
  
  gpeek        :: Ptr (f a) -> IO (f a)

  gpeekByteOff :: Ptr (f a) -> Int -> IO (f a)
  gpeekByteOff addr off = gpeek (addr `plusPtr` off)
  {-# INLINEABLE gpeekByteOff #-}
  
  gpeekElemOff :: Ptr (f a) -> Int -> IO (f a)
  gpeekElemOff addr idx = gpeek (addr `plusPtr` (idx * gsizeOf (undefined :: f a)))
  {-# INLINEABLE gpeekElemOff #-}
  
  gpoke        :: Ptr (f a) -> f a -> IO ()

  gpokeByteOff :: Ptr (f a) -> Int -> f a -> IO ()
  gpokeByteOff addr off x = gpoke (addr `plusPtr` off) x
  {-# INLINEABLE gpokeByteOff #-}
  
  gpokeElemOff :: Ptr (f a) -> Int -> f a -> IO ()
  gpokeElemOff addr idx x = gpoke (addr `plusPtr` (idx * gsizeOf (undefined :: f a))) x
  {-# INLINEABLE gpokeElemOff #-}

sizeOfDefault :: (Generic a, GStorable (Rep a)) => a -> Int
sizeOfDefault = gsizeOf . from

alignmentDefault :: (Generic a, GStorable (Rep a)) => a -> Int
alignmentDefault = galignment . from

peekDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
peekDefault ptr = return . to =<< gpeek (castPtr ptr)

peekByteOffDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> Int -> IO a
peekByteOffDefault ptr ofs = return . to =<< gpeekByteOff (castPtr ptr) ofs

peekElemOffDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> Int -> IO a
peekElemOffDefault ptr idx = return . to =<< gpeekElemOff (castPtr ptr) idx

pokeDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> a -> IO ()
pokeDefault ptr = gpoke (castPtr ptr) . from

pokeByteOffDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> Int -> a -> IO ()
pokeByteOffDefault ptr ofs = gpokeByteOff (castPtr ptr) ofs . from

pokeElemOffDefault :: (Generic a, GStorable (Rep a)) => Ptr a -> Int -> a -> IO ()
pokeElemOffDefault ptr idx = gpokeElemOff (castPtr ptr) idx . from

newtype StorableWrapper a = StorableWrapper { unStorableWrapper :: a }

instance (Generic a, GStorable (Rep a)) => Storable (StorableWrapper a) where
  sizeOf _ = gsizeOf $ from (undefined :: a)
  {-# INLINEABLE sizeOf #-}
  alignment _ = galignment $ from (undefined :: a)
  {-# INLINEABLE alignment #-}
  peek ptr = return . StorableWrapper . to =<< gpeek (castPtr ptr)
  {-# INLINEABLE peek #-}
  poke ptr (StorableWrapper v) = gpoke (castPtr ptr) $ from v
  {-# INLINEABLE poke #-}

instance GStorable U1 where
  gsizeOf _ = 0
  {-# INLINEABLE gsizeOf #-}
  gpeek _ = return U1
  {-# INLINEABLE gpeek #-}
  gpoke _ _ = return ()
  {-# INLINEABLE gpoke #-}

instance (GStorable a, GStorable b) => GStorable (a :*: b) where
  gsizeOf _ = gsizeOf (undefined :: a x) + gsizeOf (undefined :: b x)
  {-# INLINEABLE gsizeOf #-}
  gpeek ptr = do
    a <- gpeek (castPtr ptr)
    b <- gpeekByteOff (castPtr ptr) (gsizeOf a)
    return $ a :*: b
  {-# INLINEABLE gpeek #-}
  gpoke ptr (a :*: b) = do
    gpoke (castPtr ptr) a
    gpokeByteOff (castPtr ptr) (gsizeOf a) b
  {-# INLINEABLE gpoke #-}
    
instance (GStorable a, GStorable b) => GStorable (a :+: b) where
  gsizeOf _ = 4 + (gsizeOf (undefined :: a x) `max` gsizeOf (undefined :: b x))
  {-# INLINEABLE gsizeOf #-}
  gpeek ptr = do
    tag <- peek (castPtr ptr)
    if (tag :: Word32) == 0
      then return L1 `ap` gpeekByteOff (castPtr ptr) 4
      else return R1 `ap` gpeekByteOff (castPtr ptr) 4
  {-# INLINEABLE gpeek #-}
  gpoke ptr (L1 val) = poke (castPtr ptr) (0 :: Word32) >> gpokeByteOff (castPtr ptr) 4 val
  gpoke ptr (R1 val) = poke (castPtr ptr) (1 :: Word32) >> gpokeByteOff (castPtr ptr) 4 val
  {-# INLINEABLE gpoke #-}

instance (GStorable a) => GStorable (M1 i c a) where
  gsizeOf _ = gsizeOf (undefined :: a x)
  {-# INLINEABLE gsizeOf #-}
  gpeek ptr = return M1 `ap` gpeek (castPtr ptr)
  {-# INLINEABLE gpeek #-}
  gpoke ptr (M1 val) = gpoke (castPtr ptr) val
  {-# INLINEABLE gpoke #-}
    
instance (Storable a) => GStorable (K1 i a) where
  gsizeOf _ = sizeOf (undefined :: a)
  {-# INLINEABLE gsizeOf #-}
  gpeek ptr = return K1 `ap` peek (castPtr ptr)
  {-# INLINEABLE gpeek #-}
  gpoke ptr (K1 val) = poke (castPtr ptr) val
  {-# INLINEABLE gpoke #-}
