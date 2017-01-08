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

import qualified Data.Bits as Bits
import Data.Char (ord)
import Data.Word (Word8)


type Byte = Word8

data StringBuffer
  = StringBuffer
    { prevChar :: Char,
      restBytes :: [Byte],
      restString :: String }
  deriving (Show)

stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer str = StringBuffer '$' [] str

utf8Encode :: Char -> [Byte]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Bits.shiftR` 6),
                          0x80 + oc Bits..&. 0x3f ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Bits.shiftR` 12),
                          0x80 + ((oc `Bits.shiftR` 6) Bits..&. 0x3f),
                          0x80 + oc Bits..&. 0x3f ]

   | otherwise        = [ 0xf0 + (oc `Bits.shiftR` 18),
                          0x80 + ((oc `Bits.shiftR` 12) Bits..&. 0x3f),
                          0x80 + ((oc `Bits.shiftR` 6) Bits..&. 0x3f),
                          0x80 + oc Bits..&. 0x3f ]

getByte :: StringBuffer -> Maybe (Byte, StringBuffer)
getByte sb =
  case (restBytes sb, restString sb) of
    ([], []) -> Nothing
    ([], c:str) ->
      case utf8Encode c of
        b:bs -> Just (b, sb { restBytes = bs,
                              restString = str })
        [] -> Nothing
    (b:bs, _) -> Just (b, sb { restBytes = bs })

getPrevChar :: StringBuffer -> Char
getPrevChar = prevChar
{-# INLINE getPrevChar #-}

type Length = Int

getString :: Length -> StringBuffer -> String
getString l = take l . restString
{-# INLINE getString #-}
