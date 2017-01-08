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
module Language.CUTE.Parser.LexerHelper
  (
    SrcPos(..),
    ParseM(..),
    ParseState(..),
    ParseResult(..),
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

import Numeric (readOct, readDec, readHex, readInt)
import Data.Word (Word8)

------------------------------------------------------------
-- Other external imports

------------------------------------------------------------
-- CUTE language internal imports

import Language.CUTE.Parser.Token
import Language.CUTE.Parser.SrcPos
import Language.CUTE.Parser.ParseM
import Language.CUTE.Parser.StringBuffer

------------------------------------------------------------
-- Alex wrapper

data AlexInput
  = AlexInput
    { aiSrcPos :: !SrcPos,
      aiStringBuffer :: StringBuffer }
    deriving (Show)


alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput sp0 sb0) =
  do
    (b, sb1) <- getByte sb0
    return (b, AlexInput sp1 sb1)
  where sp1 = increaseSrcPos sp0


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput _ sb) = getPrevChar sb


------------------------------------------------------------
-- Concrete Action type and helper functions

type Action = SrcPos -> StringBuffer -> Length -> ParseM (Posed Token)

type Length = Int

type Offset = (Int, Int)


token :: Token -> Action
token t sp _ _ = return (Posed sp t)

tokenByString :: (String -> Token) -> Action
tokenByString tc sp sb l = return . Posed sp . tc $ str
  where
    str = getString l sb


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

type Sign = Integer -> Integer

negative, positive :: Sign
negative = negate
positive = id

tokenInteger :: Sign -> Radix -> Offset -> Action
tokenInteger s r (so, eo) sp sb l =
  return (Posed sp . CTinteger . s . extractInt $ readRadix r)
  where
    extractInt :: [(Integer, String)] -> Integer
    extractInt (readResult:_) = fst readResult
    readRadix :: Radix -> [(Integer, String)]
    readRadix rad =
      case rad of
        Binary ->
          readBin $ drop so str
        Octal ->
          readOct $ drop so str
        Decimal ->
          readDec $ drop so str
        Hexadecimal ->
          readHex $ drop so str
    readBin :: ReadS Integer
    readBin = readInt 2 isBin valBin
    isBin :: Char -> Bool
    isBin c
      | c `elem` "01" = True
      | otherwise = False
    valBin :: Char -> Int
    valBin '1' = 1
    valBin _ = 0
    str = getString l sb

-- Helper functions for String

tokenString :: Action
tokenString sp sb l = return . Posed sp . CTstring . read $ str
  where
    str = getString l sb

-- Helper functions for Comments

nestedComment :: Action
nestedComment sp sb l = go 1
  where
    go :: Int -> ParseM (Posed Token)
    go 0 = return (Posed sp $ CTstring "")
    go n = go (n-1)

