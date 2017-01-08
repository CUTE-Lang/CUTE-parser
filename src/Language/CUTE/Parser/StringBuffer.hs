module Language.CUTE.Parser.StringBuffer
  (
    Byte,
    StringBuffer,
    stringToStringBuffer,
    getByte,
    getPrevChar,
    getString,
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
  = StringBuffer
    { buffer :: !BS.ByteString,
      bufferLength :: !Length,
      bufferPosition :: !Length }

stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer str = StringBuffer bs (BS.length bs) 0
  where
    bs = BSU.fromString str
{-# INLINE stringToStringBuffer #-}

instance Show StringBuffer where
  showsPrec _ sb = showString "<StringBuffer: ("
                   . shows (bufferLength sb)
                   . showString ","
                   . shows (bufferPosition sb)
                   . showString ")="
                   . shows (showByte sb)
                   . showString " >"

showByte :: StringBuffer -> Maybe Byte
showByte sb =
  if (bufferPosition sb < bufferLength sb)
  then Just $ BS.index (buffer sb) (bufferPosition sb)
  else Nothing
{-# INLINE showByte #-}

skipBuffer :: Length -> StringBuffer -> StringBuffer
skipBuffer n sb@(StringBuffer _ _ bp) = sb {bufferPosition = bp + n}
{-# INLINE skipBuffer #-}

getByte :: StringBuffer -> Maybe (Byte, StringBuffer)
getByte sb0 =
  do
    b <- showByte sb0
    let sb1 = skipBuffer 1 sb0
    return (b, sb1)
{-# INLINE getByte #-}

getPrevChar :: StringBuffer -> Maybe Char
getPrevChar sb = Just 'c'
{-# INLINE getPrevChar #-}

getString :: Length -> StringBuffer -> String
getString l sb = toString . BSU.take l
                 . BS.drop (bufferPosition sb) $ buffer sb
{-# INLINE getString #-}

getChar :: StringBuffer -> Maybe Char
getChar sb =
  do
    (c,_) <- BSU.decode $ buffer sb
    return c
{-# INLINE getChar #-}
