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
    lexError,
    token,
    tokenByString,
    Radix,
    binary, octal, decimal, hexadecimal,
    Sign,
    negative, positive,
    tokenInteger,
    tokenString,
    nestedComment
  )
where

------------------------------------------------------------
-- Standard base imports

import Numeric (readOct, readDec, readHex, readInt)
import Data.Word (Word8)

------------------------------------------------------------
-- Other external imports

import qualified Data.ByteString.UTF8 as BSU

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
    { alexCurrPos :: !SrcPos,
      alexStringBuffer :: StringBuffer }
    deriving (Show)


alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput sp0 sb0) =
  do
    (c, sb1) <- getBufferChar sb0
    let l = getBufferPosition sb1 - getBufferPosition sb0
        sp1 = increaseSrcPos l sp0
    return (c, AlexInput sp1 sb1)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput sp0 sb0) =
  do
    (b, sb1) <- getBufferByte sb0
    return (b, AlexInput sp1 sb1)
  where sp1 = increaseSrcPos 1 sp0

-- Define this when use left ctx of Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = undefined

getAlexInput :: ParseM AlexInput
getAlexInput =
  do
    ps <- getParseState
    let sb = stringBuffer ps
        cp = currPos ps
        ai = AlexInput cp sb
    return ai

putAlexInput :: AlexInput -> ParseM ()
putAlexInput ai =
  do
    ps0 <- getParseState
    let sb = alexStringBuffer ai
        cp = alexCurrPos ai
        ps1 = ps0 { stringBuffer = sb, currPos = cp }
    putParseState ps1

------------------------------------------------------------
-- Concrete Action type and helper functions

type Action = SrcPos -> StringBuffer -> Length -> ParseM (Posed Token)

type Length = Int

type Offset = (Int, Int)

lexError :: String -> Action
lexError msg _ _ _ = failParseM msg

token :: Token -> Action
token t sp _ _ = return (Posed sp t)

tokenByString :: (String -> Token) -> Action
tokenByString tc sp sb l = return . Posed sp . tc $ str
  where
    str = getBufferString l sb


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
    str = getBufferString l sb

-- Helper functions for String

tokenString :: Action
tokenString sp sb l = return . Posed sp . CTstring . read $ str
  where
    str = getBufferString l sb

-- Helper functions for Comments

nestedComment :: Action
nestedComment sp _ _ =
  do
    alexI0 <- getAlexInput
    go 1 "" alexI0
  where
    errMsg :: String
    errMsg = "EOF while parsing comment"
    go :: Int -> String -> AlexInput -> ParseM (Posed Token)
    go 0 str0 alexI0 =
      return (Posed sp . CTcomment . reverse . drop 2 $ str0)
    go n str0 alexI0 =
      do
        (c0, alexI1) <- liftMaybe errMsg $ alexGetChar alexI0
        putAlexInput alexI1
        let str1 = c0:str0
        case c0 of
          '/' ->
            do
              (c1, alexI2) <- liftMaybe errMsg $ alexGetChar alexI1
              putAlexInput alexI2
              let str2 = c1:str1
              case c1 of
                '*' ->
                  go (n+1) str2 alexI2
                _ ->
                  go n str2 alexI2
          '*' ->
            do
              (c1, alexI2) <- liftMaybe errMsg $ alexGetChar alexI1
              putAlexInput alexI2
              let str2 = c1:str1
              case c1 of
                '/' ->
                  go (n-1) str2 alexI2
                _ ->
                  go n str2 alexI2
          _ ->
            go n str1 alexI1
