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
-- Proposition types and typeclasses, and instances.
--
module Language.CUTE.Parser.LexerHelper
  (
    SrcPos(..),
    Byte,
    AlexInput(..),
    alexGetByte,
    alexInputPrevChar,
    Action,
    token,
    Radix,
    binary, octal, decimal, hexadecimal,
    Sign,
    negative, positive,
    tokenInteger
  )
where

------------------------------------------------------------
-- Standard base imports

import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits

------------------------------------------------------------
-- Other external imports

------------------------------------------------------------
-- CUTE language internal imports

import Language.CUTE.Parser.Token

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
-- Src position type and helper function

data SrcPos
  = SrcPos
    { spFilename :: !String,
      spOffset :: !Int }
    deriving (Eq,Ord,Show)


increaseSrcPos :: SrcPos -> SrcPos
increaseSrcPos sp =
  case sp of
    SrcPos f o -> SrcPos f (o + 1)

------------------------------------------------------------
-- Alex wrapper

data AlexInput
  = AlexInput
    { aiSrcPos :: !SrcPos,
      aiPrevChar :: Char,
      aiRestByte :: [Byte],
      aiRestInput :: String }
    deriving (Show)


alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput sp pc bs cs) =
  case (bs, cs) of
    ([], []) -> Nothing
    ([], c:cs) ->
      case utf8Encode c of
        b:bs -> Just (b, AlexInput sp' pc bs cs)
        [] -> Nothing
    (b:bs, _) -> Just (b, AlexInput sp' pc bs cs)
  where sp' = increaseSrcPos sp


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai =
  case ai of
    (AlexInput _ pc _ _) -> pc


------------------------------------------------------------
-- Concrete Action type and helper functions

type Action = SrcPos -> Length -> String -> (SrcPos, Token)

type Length = Int

type Offset = (Int, Int)


token :: Token -> Action
token t sp l str = (sp, t)


type Radix = Int

binary, octal, decimal, hexadecimal :: Radix
binary = 2
octal = 8
decimal = 10
hexadecimal = 16

type Sign  = Integer -> Integer

negative, positive :: Sign
negative = negate
positive = id

tokenInteger :: Sign -> Radix -> Offset -> Action
tokenInteger s r (so, eo) sp l str = (sp, CTinteger $ s $ read str)
