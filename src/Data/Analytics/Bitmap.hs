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
  , fromForeignPtr
  , fromVector, toVector
  , fromSeq, toSeq
  , createAndTrim
  , create
  , create'
  , unsafeCreate
  , (!)
  , (//)
  , word
  , take
  , mmap
  ) where

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Control.Exception (assert)
import Control.Lens

import Data.Analytics.Dictionary
import Data.Bits
import Data.Bits.Lens
import Data.Binary as Binary
import Data.Bytes.Serial as Bytes
import Data.Bytes.Get as Bytes
import Data.Bytes.Put as Bytes
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Serialize as Serial
import Data.ByteString hiding (take, empty, null)
import Data.ByteString.Internal (ByteString(..))
import Data.Data
import qualified Data.Foldable as Foldable
import Data.Monoid
import qualified Data.Vector.Storable as Storable

import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable

import GHC.Base (realWorld#, nullAddr#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), mallocPlainForeignPtrBytes)
import GHC.IO (IO(..), unsafeDupablePerformIO)

import Prelude hiding (length, null, take)
import qualified Prelude

import System.IO.MMap

import Text.Read

data Bitmap = Bitmap
  {-# UNPACK #-} !(ForeignPtr Word64) -- payload
  {-# UNPACK #-} !Int                 -- offset in 'Word64's
  {-# UNPACK #-} !Int                 -- length in /bits/
  deriving Typeable

fromForeignPtr :: ForeignPtr Word64 -> Int -> Int -> Bitmap
fromForeignPtr = Bitmap
{-# INLINE fromForeignPtr #-}

fromVector :: Storable.Vector Word64 -> Int -> Bitmap
fromVector v i = case Storable.unsafeToForeignPtr v of
  (fp, o, l) -> Bitmap fp o $ min i (shiftL l 6)
{-# INLINE fromVector #-}

toVector :: Bitmap -> Storable.Vector Word64
toVector (Bitmap fp o l) = Storable.unsafeFromForeignPtr fp o $ shiftR (l + 7) 6
{-# INLINE toVector #-}

-- | Uses a virtual constructor to present the contents as '[Bool]'
instance Data Bitmap where
  gfoldl f z b = z fromList `f` (toList b)
  toConstr _ = fromListConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _ = bitmapDataType

fromListConstr :: Constr
fromListConstr = mkConstr bitmapDataType "fromList" [] Prefix
{-# NOINLINE fromListConstr #-}

bitmapDataType :: DataType
bitmapDataType = mkDataType "Data.Analytics.Bitmap.Bitmap" [fromListConstr]
{-# NOINLINE bitmapDataType #-}

-- | Generate a bytestring for all of the bits in a 'Bitmap'.
--
-- You may get garbage bits for bits in the last byte that are beyond the
-- length of the 'Bitmap'.
toByteString :: Bitmap -> ByteString
toByteString (Bitmap fp o l) = PS (castForeignPtr fp) (shiftL o 3) $ shiftR (l + 7) 3
{-# INLINE toByteString #-}

-- | Construct a 'Bitmap' from a 'ByteString'. If the 'ByteString' isn't an even multiple
-- of @8@ bytes and isn't 8-byte aligned, then it will be copied.
fromByteString :: ByteString -> Bitmap
fromByteString bs@(PS fp o l)
  | l .&. 8 == 0 && o .&. 8 == 0 = Bitmap (castForeignPtr fp) (shiftR o 3) (shiftL l 3)
  | otherwise = case copy bs of
    PS fp' 0 _ -> Bitmap (castForeignPtr fp') 0 (shiftL l 3)
    _          -> error "Data.Analytics.Bitmap.fromByteString: copy failed to copy"
{-# INLINE fromByteString #-}

instance Binary Bitmap where
  get = deserialize
  put = serialize

instance Serialize Bitmap where
  get = deserialize
  put = serialize

instance Serial Bitmap where
  deserialize = do
    l <- deserialize
    bs <- Bytes.getByteString (shiftR (l + 7) 3)
    return $! take l (fromByteString bs)
  serialize b = do
    serialize (size b)
    Bytes.putByteString (toByteString b)

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
  a@(Bitmap fp o len) == b@(Bitmap fp' o' len')
    | len /= len'          = False
    | fp == fp' && o == o' = True
    | otherwise            = compare a b == EQ
  {-# INLINE (==) #-}

instance Ord Bitmap where
  compare (Bitmap _ _ 0) (Bitmap _ _ 0) = EQ
  compare (Bitmap fp1 o1 len1) (Bitmap fp2 o2 len2) =
    inlinePerformIO $
      withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> let
          w1 = shiftR (len1 - 1) 6 -- # of /full/ words in a
          w2 = shiftR (len2 - 1) 6 -- # of /full/ words in b
          q1 = plusPtr p1 o1
          q2 = plusPtr p2 o2
        in memcmp q1 q2 (min w1 w2 * 8) >>= \ i -> case compare i 0 of
          LT -> return LT
          EQ -> case len1 .&. 63 of
            0 -> return $! compare len1 len2 -- no extra bits to compare, just compare remaining lengths
            r1 -> case len2 .&. 63 of
              0 -> return $! compare len1 len2 -- no extra bits to compare, just compare remaining lengths
              r2 -> do
               e1 <- peekElemOff q1 w1
               e2 <- peekElemOff q2 w2
               let mask :: Word64
                   mask = bit (min r1 r2) - 1
               return $! compare (e1 .&. mask) (e2 .&. mask) <> compare len1 len2
          GT -> return GT

-- | /O(n)/
instance Dictionary Bool Bitmap where
  size (Bitmap _ _ l) = l
  {-# INLINE size #-}

  rank True b o = rank1 b o
  rank False b@(Bitmap _ _ l) o = min o l - rank1 b o
  {-# INLINE rank #-}

  select a b o = select a (b^._Bitmap) o
  {-# INLINE select #-}

rank1 :: Bitmap -> Int -> Int
rank1 (Bitmap fp d l) o
  | m > 0 = inlinePerformIO $ withForeignPtr fp $ \p -> let q = plusPtr p d in go 0 w q >>= \t ->
    if b == 0
    then return t
    else peekElemOff q (w + d) >>= \e -> return $! t + popCount (e .&. mask)
  | otherwise = 0
  where
    m = min o l
    w = shiftR (m - 1) 6 -- # of /full/ words we need to count.
    b = m .&. 63
    mask = bit b - 1 :: Word64
    go !acc 0  !_ = return acc
    go !acc !r !p = peek p >>= \e -> go (acc + popCount e) (r - 1) (plusPtr p 1)

instance NFData Bitmap

fromSeq :: Seq Bool -> Bitmap
fromSeq xs = unsafeCreate (Seq.length xs) (go xs) where
  go ys p
    | Seq.null ys = return ()
    | otherwise = case Seq.splitAt 64 ys of
      (as,bs) -> do
        poke p (0 & partsOf bits .~ Foldable.toList as)
        go bs (plusPtr p 1)

toSeq :: Bitmap -> Seq Bool
toSeq xs = case Seq.replicateA (size xs) (El 1 (xs !)) of
  El _ f -> f 0

data El a = El {-# UNPACK #-} !Int (Int -> a)

instance Functor El where
  fmap f (El i g) = El i (f . g)

instance Applicative El where
  pure a = El 0 (const a)
  El i f <*> El j g = El (i + j) $ \o -> f o $ g (o + i)

fromList :: [Bool] -> Bitmap
fromList xs = unsafeCreate (Prelude.length xs) (go xs) where
  go [] _ = return ()
  go ys p = case Prelude.splitAt 64 ys of -- this could be more efficient but this is at least obvious
    (as,bs) -> do
      poke p (0 & partsOf bits .~ as)
      go bs (plusPtr p 1)
{-# INLINE fromList #-}

toList :: Bitmap -> [Bool]
toList b = Prelude.take l $ do
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
  ix o f b@(Bitmap fp d l)
    | o < 0  = pure b
    | o >= l = pure b
    | l == 0 = pure b
    | otherwise = inlinePerformIO $ withForeignPtr fp $ \p -> do
      w <- peekElemOff (plusPtr p d) (shiftR o 6)
      return $! indexed f o (testBit (w :: Word64) (o .&. 63)) <&> \r -> b // [(o,r)]

-- | Returns 'False' when indexing out of bounds
(!) :: Bitmap -> Int -> Bool
(!) (Bitmap fp d l) o
  | o < 0  = False
  | o >= l = False
  | l == 0 = False
  | otherwise = inlinePerformIO $ withForeignPtr fp $ \p -> do
    w <- peekElemOff (plusPtr p d) (shiftR o 6)
    return $! testBit (w :: Word64) (o .&. 63)
{-# INLINE (!) #-}

(//) :: Bitmap -> [(Int, Bool)] -> Bitmap
Bitmap fp d l // os = inlinePerformIO $ withForeignPtr fp $ \p ->
  create l $ \p' -> do
    memcpy p' (plusPtr p d) l
    Foldable.for_ os $ \(i,b) ->
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
word o (Bitmap fp d l)
  | o < 0  = 0
  | l == 0 = 0
  | otherwise = case compare o m of
    LT -> inlinePerformIO $ withForeignPtr fp $ \p -> peekElemOff p (o + d)
    EQ -> inlinePerformIO $ withForeignPtr fp $ \p -> do
      r <- peekElemOff p (o + d)
      return $! r .&. (bit (l .&. 63) - 1)
    GT -> 0
  where m = shiftR (l + 63) 6 - 1
{-# INLINE word #-}

-- @'unsigned' o l@ should decode @l@ bits starting at offset @o@ as an unsigned @l@ bit number and return it.
-- unsigned :: Int -> Int -> Bitmap -> Integer

-- | Return whether or not a given bit vector is empty
null :: Bitmap -> Bool
null (Bitmap _ _ l) = l == 0
{-# INLINE null #-}

-- | The 'empty' bit vector.
empty :: Bitmap
empty = Bitmap nullForeignPtr 0 0

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
  return $! Bitmap fp 0 l
{-# INLINE create #-}

-- | @'create'' l f@ creates a bit vector of up to @l@ bytes and uses the action @f@ to fill it and obtain its true size.
create' :: Int -> (Ptr Word64 -> IO (Int, a)) -> IO (Bitmap, a)
create' l f = do
    fp <- mallocPlainForeignPtrBytes (bytesRequired l)
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return $! (Bitmap fp 0 l', res)
{-# INLINE create' #-}

createAndTrim :: Int -> (Ptr Word64 -> IO Int) -> IO Bitmap
createAndTrim l f = do
  let bl = bytesRequired l
  fp <- mallocPlainForeignPtrBytes (bytesRequired l) -- bits to bytes
  withForeignPtr fp $ \ p -> do
    l' <- f p
    let bl' = bytesRequired l'
    if assert (bl' <= bl) (bl' >= bl)
      then return $! Bitmap fp 0 l
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

-- | /O(1)/ @take n b@ takes the first @n@ bits of @b@.
take :: Int -> Bitmap -> Bitmap
take n (Bitmap fp o l)
  | n > 0     = Bitmap fp o (min (max n 0) l)
  | otherwise = empty

foreign import ccall unsafe "string.h memcmp" c_memcmp :: Ptr a -> Ptr a -> CSize -> IO CInt

memcmp :: Ptr a -> Ptr a -> Int -> IO CInt
memcmp p q s = c_memcmp p q (fromIntegral s)

foreign import ccall unsafe "string.h memcpy" c_memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

memcpy :: Ptr a -> Ptr a -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

-- | 'mmap' the contents of a file into memory as a 'Bitmap', automatically unmapping when this goes out of scope.
--
-- This is somewhat risky in that if the contents of the file are edited while you have it open then the purity of your
-- program is in jeopardy.
mmap :: FilePath -> IO Bitmap
mmap path = do
  (fp,offset,sz) <- mmapFileForeignPtr path ReadOnly Nothing
  assert (offset .&. 64 == 0) $ return $! Bitmap fp (shiftR offset 3) (shiftL sz 3)
