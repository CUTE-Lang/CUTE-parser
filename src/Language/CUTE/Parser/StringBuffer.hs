module Language.CUTE.Parser.StringBuffer
  (
    Byte
  , StringBuffer
  , stringToStringBuffer
  , skipBuf
  , getBufByte
  , getBufStr
  , getBufChar
  , getBufLen
  , getBufPos
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word8)

import Codec.Binary.UTF8.String as UTF8
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU

type Byte = Word8

type Length = Int

data StringBuffer
  = StringBuffer {
      bufByteStr :: !BS.ByteString
    , bufLen :: !Length
    , bufPos :: !Length
    }

stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer str = StringBuffer bs (BS.length bs) 0
  where
    bs = BSU.fromString str
{-# INLINE stringToStringBuffer #-}

instance Show StringBuffer where
  showsPrec _ buf =
    showString "<StringBuffer: ("
      . shows (bufLen buf)
      . showString ","
      . shows (bufPos buf)
      . showString ")="
      . shows (showByte buf)
      . showString " >"

showByte :: StringBuffer -> Maybe Byte
showByte buf =
  if bufPos buf < bufLen buf
  then Just $ BS.index (bufByteStr buf) (bufPos buf)
  else Nothing
{-# INLINE showByte #-}

skipBuf :: Length -> StringBuffer -> StringBuffer
skipBuf n buf@(StringBuffer _ _ bp) = buf {bufPos = bp + n}
{-# INLINE skipBuf #-}

getBufByte :: StringBuffer -> Maybe (Byte, StringBuffer)
getBufByte buf =
  do
    b <- showByte buf
    return (b, skipBuf 1 buf)
{-# INLINE getBufByte #-}

getBufStr :: Length -> StringBuffer -> String
getBufStr len buf =
  toString . BSU.take len . BS.drop (bufPos buf) $ bufByteStr buf
{-# INLINE getBufStr #-}

getBufChar :: StringBuffer -> Maybe (Char, StringBuffer)
getBufChar buf =
  do
    (c,l) <- BSU.decode . BS.drop (bufPos buf) $ bufByteStr buf
    return (c, skipBuf l buf)
{-# INLINE getBufChar #-}

getBufLen :: StringBuffer -> Length
getBufLen = bufLen
{-# INLINE getBufLen #-}

getBufPos :: StringBuffer -> Length
getBufPos = bufPos
{-# INLINE getBufPos #-}
