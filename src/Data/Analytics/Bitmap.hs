{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Bitmap
  ( Bitmap(..)
  , _Bitmap
  , null
  , empty
  , HasBitmap(..)
  , fromForeignPtr
  , createAndTrim
  , create
  , create'
  , unsafeCreate
  , (!)
  , (//)
  , word
  ) where

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Control.Exception (assert)
import Control.Lens

import Data.Analytics.Dictionary
import Data.Bits
import Data.Bits.Lens
import Data.Foldable hiding (toList)
import Data.Typeable
import Data.Monoid
import Data.Word

import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable

import GHC.Base (realWorld#, nullAddr#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), mallocPlainForeignPtrBytes)
import GHC.IO (IO(..), unsafeDupablePerformIO)

import Prelude hiding (length, null)
import qualified Prelude

import Text.Read

data Bitmap = Bitmap
  { _bitmapWords  :: {-# UNPACK #-} !(ForeignPtr Word64) -- payload
  , _bitmapLength :: {-# UNPACK #-} !Int                 -- length in /bits/
  } deriving Typeable

makeClassy ''Bitmap

fromForeignPtr :: ForeignPtr Word64 -> Int -> Bitmap
fromForeignPtr = Bitmap

instance Show Bitmap where
  showsPrec d b = showParen (d > 8) $
    showString "_Bitmap # " . showsPrec 8 (toList b)

instance Read Bitmap where
  readPrec = parens $ prec 8 $ do
    Ident "_Bitmap" <- step lexP
    Symbol "#" <- lexP
    r <- step readPrec
    return $! fromList r

instance Eq Bitmap where
  a@(Bitmap fp len) == b@(Bitmap fp' len')
    | len /= len' = False
    | fp == fp'   = True
    | otherwise = compare a b == EQ
  {-# INLINE (==) #-}

instance Ord Bitmap where
  compare (Bitmap _ 0) (Bitmap _ 0) = EQ
  compare (Bitmap fp1 len1) (Bitmap fp2 len2) =
    inlinePerformIO $
      withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> let
          w1 = shiftR (len1 - 1) 6 -- # of /full/ words in a
          w2 = shiftR (len2 - 1) 6 -- # of /full/ words in b
        in memcmp p1 p2 (min w1 w2 * 8) >>= \ i -> case compare i 0 of
          LT -> return LT
          EQ -> case len1 .&. 63 of
            0 -> return $! compare len1 len2 -- no extra bits to compare, just compare remaining lengths
            r1 -> case len2 .&. 63 of
              0 -> return $! compare len1 len2 -- no extra bits to compare, just compare remaining lengths
              r2 -> do
               e1 <- peekElemOff p1 w1
               e2 <- peekElemOff p2 w2
               let mask = bit (min r1 r2) - 1
               return $! compare (e1 .&. mask) (e2 .&. mask) <> compare len1 len2
          GT -> return GT

-- | /O(n)/
instance Dictionary Bool Bitmap where
  size (Bitmap _ l) = l
  {-# INLINE size #-}

  rank True b o = rank1 b o
  rank False b@(Bitmap _ l) o = min o l - rank1 b o
  {-# INLINE rank #-}

  select a b o = select a (b^._Bitmap) o
  {-# INLINE select #-}

rank1 :: Bitmap -> Int -> Int
rank1 (Bitmap fp l) o
  | m > 0 = inlinePerformIO $ withForeignPtr fp $ \p -> go 0 w p >>= \t ->
    if b == 0
    then return t
    else peekElemOff p w >>= \e -> return $! t + popCount (e .&. mask)
  | otherwise = 0
  where
    m = min o l
    w = shiftR (m - 1) 6 -- # of /full/ words we need to count.
    b = m .&. 63
    mask = bit b - 1
    go !acc 0  !_ = return acc
    go !acc !r !p = peek p >>= \e -> go (acc + popCount e) (r - 1) (p `plusPtr` 1)

instance NFData Bitmap

fromList :: [Bool] -> Bitmap
fromList xs = unsafeCreate (Prelude.length xs) (go xs) where
  go [] _ = return ()
  go ys p = case splitAt 64 ys of -- this could be more efficient but this is at least obvious
    (as,bs) -> do
      poke p (0 & partsOf bits .~ as)
      go bs (p `plusPtr` 1)
{-# INLINE fromList #-}

toList :: Bitmap -> [Bool]
toList b = take l $ do
  i <- [ 0 .. wordsRequired l - 1 ]
  word i b ^.. bits
  where l = size b
{-# INLINE toList #-}

-- | This isomorphism can be used to convert from a 'Bitmap' to a list of booleans (and back).
_Bitmap :: Iso' Bitmap [Bool]
_Bitmap = iso toList fromList
{-# INLINE _Bitmap #-}

type instance Index Bitmap = Int

instance Applicative f => Each f Bitmap Bitmap Bool Bool where
  each = _Bitmap.traversed

instance (Functor f, Contravariant f) => Contains f Bitmap where
  contains = containsLength size

type instance IxValue Bitmap = Bool
instance Applicative f => Ixed f Bitmap where
  ix o f b@(Bitmap fp l)
    | o < 0  = pure b
    | o >= l = pure b
    | l == 0 = pure b
    | otherwise = inlinePerformIO $ withForeignPtr fp $ \p -> do
      w <- peekElemOff p (shiftR o 6)
      return $! indexed f o (testBit w (o .&. 63)) <&> \r -> b // [(o,r)]

-- | Returns 'False' when indexing out of bounds
(!) :: Bitmap -> Int -> Bool
(!) (Bitmap fp l) o
  | o < 0  = False
  | o >= l = False
  | l == 0 = False
  | otherwise = inlinePerformIO $ withForeignPtr fp $ \p -> do
    w <- peekElemOff p (shiftR o 6)
    return $! testBit w (o .&. 63)
{-# INLINE (!) #-}

(//) :: Bitmap -> [(Int, Bool)] -> Bitmap
Bitmap fp l // os = inlinePerformIO $ withForeignPtr fp $ \p ->
  create l $ \p' -> do
    memcpy p' p l
    for_ os $ \(i,b) ->
      if 0 <= i && i < l
      then do
        let q = shiftR i 6
            r = i .&. 63
            p'' = plusPtr p' q
        w <- peek p''
        poke p'' (if b then setBit w r else clearBit w r :: Word64)
      else return ()

-- | Returns 0 when indexing out of bounds
word :: Int -> Bitmap -> Word64
word o (Bitmap fp l)
  | o < 0  = 0
  | l == 0 = 0
  | otherwise = case compare o m of
    LT -> inlinePerformIO $ withForeignPtr fp $ \p -> peekElemOff p o
    EQ -> inlinePerformIO $ withForeignPtr fp $ \p -> do
      r <- peekElemOff p o
      return $! r .&. (bit (l .&. 63) - 1)
    GT -> 0
  where m = shiftR (l + 63) 6 - 1
{-# INLINE word #-}

-- @'unsigned' o l@ should decode @l@ bits starting at offset @o@ as an unsigned @l@ bit number and return it.
-- unsigned :: Int -> Int -> Bitmap -> Integer

-- | Return whether or not a given bit vector is empty
null :: Bitmap -> Bool
null (Bitmap _ l) = l == 0
{-# INLINE null #-}

-- | The 'empty' bit vector.
empty :: Bitmap
empty = Bitmap nullForeignPtr 0

-- | @'wordsRequired' n@ returns the number of bytes required to store a bit vector with @n@ bits.
wordsRequired :: Int -> Int
wordsRequired bs = shiftR (bs + 63) 6 `max` 0
{-# INLINE wordsRequired #-}

-- | @'bytesRequired' n@ returns the number of bytes required to store a bit vector with @n@ bits.
bytesRequired :: Int -> Int
bytesRequired bs = wordsRequired bs * 8
{-# INLINE bytesRequired #-}

-- | @'create' l f@ creates a bit vector of size @l@ and uses action @f@ to fill it.
create :: Int -> (Ptr Word64 -> IO ()) -> IO Bitmap
create l f = do
  fp <- mallocPlainForeignPtrBytes (bytesRequired l) -- bits to bytes
  withForeignPtr fp f
  return $! Bitmap fp l
{-# INLINE create #-}

-- | @'create'' l f@ creates a bit vector of up to @l@ bytes and uses the action @f@ to fill it and obtain its true size.
create' :: Int -> (Ptr Word64 -> IO (Int, a)) -> IO (Bitmap, a)
create' l f = do
    fp <- mallocPlainForeignPtrBytes (bytesRequired l)
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return $! (Bitmap fp l', res)
{-# INLINE create' #-}

createAndTrim :: Int -> (Ptr Word64 -> IO Int) -> IO Bitmap
createAndTrim l f = do
  let bl = bytesRequired l
  fp <- mallocPlainForeignPtrBytes (bytesRequired l) -- bits to bytes
  withForeignPtr fp $ \ p -> do
    l' <- f p
    let bl' = bytesRequired l'
    if assert (bl' <= bl) (bl' >= bl)
      then return $! Bitmap fp l
      else create l' $ \p' -> memcpy p' p bl'
{-# INLINE createAndTrim #-}

unsafeCreate :: Int -> (Ptr Word64 -> IO ()) -> Bitmap
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | The 0 pointer. Used to indicate the empty Bytestring.
nullForeignPtr :: ForeignPtr Word64
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

foreign import ccall unsafe "string.h memcmp" c_memcmp :: Ptr a -> Ptr a -> CSize -> IO CInt

memcmp :: Ptr a -> Ptr a -> Int -> IO CInt
memcmp p q s = c_memcmp p q (fromIntegral s)

foreign import ccall unsafe "string.h memcpy" c_memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

memcpy :: Ptr a -> Ptr a -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()
