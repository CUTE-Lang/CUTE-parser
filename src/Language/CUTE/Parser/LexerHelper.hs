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


-- Required functions for Alex
alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput pos0 buf0) =
  do
    (b, buf1) <- getBufByte buf0
    return (b, AlexInput pos1 buf1)
  where
    pos1 = incSrcPos 1 pos0

-- Define this when use left ctx of Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = undefined

------------------------------------------------------------
-- Utility functions for Alex

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput pos0 buf0) =
  do
    (c, buf1) <- getBufChar buf0
    let l = getBufPos buf1 - getBufPos buf0
        pos1 = incSrcPos l pos0
    return (c, AlexInput pos1 buf1)

getAlexInput :: ParseM AlexInput
getAlexInput =
  do
    ps <- getParseState
    let
      buf = strBuf ps
      cpos = currPos ps
    return $ AlexInput cpos buf

putAlexInput :: AlexInput -> ParseM ()
putAlexInput (AlexInput pos buf) =
  do
    ps0 <- getParseState
    let
      ps1 = ps0 { strBuf = buf, currPos = pos }
    putParseState ps1

------------------------------------------------------------
-- Concrete Action type and helper functions

type Action = SrcPos -> StringBuffer -> Length -> ParseM (Posed Token)
type Length = Int
type Offset = (Int, Int)

-- Generic helper functions

lexError :: String -> Action
lexError msg _ _ _ = failParseM msg

token :: Token -> Action
token t pos _ _ = return (Posed pos t)

tokenByString :: (String -> Token) -> Action
tokenByString tc pos buf len = return . Posed pos . tc $ str
  where
    str = getBufStr len buf


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
tokenInteger sign radix offsets pos buf len =
  return . Posed pos . CTinteger . sign . extractInt $ readRadix radix
  where
    (startOffset, endOffset) = offsets
    extractInt :: [(Integer, String)] -> Integer
    extractInt (readResult:_) = fst readResult
    readRadix rad =
      case rad of
        Binary ->
          readBin $ drop startOffset str
        Octal ->
          readOct $ drop startOffset str
        Decimal ->
          readDec $ drop startOffset str
        Hexadecimal ->
          readHex $ drop startOffset str
    readBin = readInt 2 isBin valBin
    isBin c
      | c `elem` "01" = True
      | otherwise = False
    valBin '1' = 1
    valBin _ = 0
    str = getBufStr len buf

-- Helper functions for String

tokenString :: Action
tokenString pos buf len =
  return . Posed pos . CTstring . read $ str
  where
    str = getBufStr len buf

-- Helper functions for Comments

nestedComment :: Action
nestedComment pos _ _ =
  do
    aInp <- getAlexInput
    go 1 "" aInp
  where
    errMsg :: String
    errMsg = "EOF while parsing comment"
    go :: Int -> String -> AlexInput -> ParseM (Posed Token)
    go 0 str0 aInp0 =
      return (Posed pos . CTcomment . reverse . drop 2 $ str0)
    go n str0 aInp0 =
      do
        (c0, aInp1) <- liftMaybe errMsg $ alexGetChar aInp0
        putAlexInput aInp1
        let str1 = c0:str0
        if c0 == '/' || c0 == '*'
        then
            do
              (c1, aInp2) <- liftMaybe errMsg $ alexGetChar aInp1
              putAlexInput aInp2
              let str2 = c1:str1
              case (c0, c1) of
                ('/', '*') ->
                  go (n + 1) str2 aInp2
                ('*', '/') ->
                  go (n - 1) str2 aInp2
                _ ->
                  go n str2 aInp2
        else
          go n str1 aInp1
