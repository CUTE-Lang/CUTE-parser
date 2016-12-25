-- |
-- Module      : Language.CUTE.Parser.LexerHelper
-- Description :
-- Copyright   : (c) CUTE Lang, 2016-
-- License     : see LICENSE
-- Maintainer  : Junyoung Clare Jang <jjc9310@gmail.com>
--             , Youngmin Cho <>
--             , Na Yeon Park <>
-- Portability : Windows, POSIX
--
{-# LANGUAGE BinaryLiterals #-}
module Language.CUTE.Parser.LexerHelper
  (
    SrcPos(..),
    Byte,
    AlexInput(..),
    alexGetByte,
    alexInputPrevChar,
    Action,
    token,
    tokenByString,
    Radix,
    binary, octal, decimal, hexadecimal,
    Sign,
    negative, positive,
    tokenInteger,
    tokenString
  )
where

------------------------------------------------------------
-- Standard base imports

import Numeric (readOct, readDec, readHex)
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
import Text.Read.Lex (readIntP)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

------------------------------------------------------------
-- Other external imports

------------------------------------------------------------
-- CUTE language internal imports

import Language.CUTE.Parser.Token
import Language.CUTE.Parser.SrcPos
import Language.CUTE.Parser.ParseM

------------------------------------------------------------
-- Byte type and helper function

type Byte = Word8


utf8Encode :: Char -> [Byte]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]


------------------------------------------------------------
-- Alex wrapper

-- TODO: Use ByteString
data AlexInput
  = AlexInput
    { aiSrcPos :: !SrcPos,
      aiPrevChar :: Char,
      aiRestByte :: [Byte],
      aiRestInput :: String }
    deriving (Show)


alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput sp pc bs str) =
  case (bs, str) of
    ([], []) -> Nothing
    ([], c:str) ->
      case utf8Encode c of
        b:bs -> Just (b, AlexInput sp' pc bs str)
        [] -> Nothing
    (b:bs, _) -> Just (b, AlexInput sp' pc bs str)
  where sp' = increaseSrcPos sp


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai =
  case ai of
    (AlexInput _ pc _ _) -> pc


------------------------------------------------------------
-- Concrete Action type and helper functions

-- TODO: Use ParseM
type Action = SrcPos -> Length -> String -> (SrcPos, Token)

type Length = Int

type Offset = (Int, Int)


token :: Token -> Action
token t sp l str = (sp, t)

tokenByString :: (String -> Token) -> Action
tokenByString tc sp l str = (sp, tc str)


-- Helper functions for Integer

data Radix
  = Binary
  | Octal
  | Decimal
  | Hexadecimal

binary, octal, decimal, hexadecimal :: Radix
binary = Binary
octal = Octal
decimal = Decimal
hexadecimal = Hexadecimal

type Sign  = Integer -> Integer

negative, positive :: Sign
negative = negate
positive = id

tokenInteger :: Sign -> Radix -> Offset -> Action
tokenInteger s r (so, eo) sp l str =
  case r of
    Binary ->
      (sp, CTinteger . extractInt . readBin $ drop so str)
    Octal ->
      (sp, CTinteger . extractInt . readOct $ drop so str)
    Decimal ->
      (sp, CTinteger . extractInt . readDec $ drop so str)
    Hexadecimal ->
      (sp, CTinteger . extractInt . readHex $ drop so str)
  where
    extractInt :: [(Integer, String)] -> Integer
    extractInt (readResult:_) = fst readResult
    readBin :: ReadS Integer
    readBin = readP_to_S readBinP
    readBinP :: ReadP Integer
    readBinP = readIntP 2 isBin valBin
    isBin :: Char -> Bool
    isBin c
      | c `elem` "01" = True
      | otherwise = False
    valBin :: Char -> Int
    valBin '1' = 1
    valBin _ = 0

-- Helper functions for String

tokenString :: Action
tokenString sp l str = (sp, CTstring $ read str)
