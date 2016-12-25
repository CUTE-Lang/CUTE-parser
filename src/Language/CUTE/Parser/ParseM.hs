-- |
-- Module      : Language.CUTE.Parser.ParseM
-- Description :
-- Copyright   : (c) CUTE Lang, 2016-
-- License     : see LICENSE
-- Maintainer  : Junyoung Clare Jang <jjc9310@gmail.com>
--             , Youngmin Cho <>
--             , Na Yeon Park <>
-- Portability : Windows, POSIX
--
module Language.CUTE.Parser.ParseM
  (
    ParseState(..),
    ParseResult(..),
    ParseM(..),
    returnParseM,
    thenParseM,
    failParseM,
    getParseState,
  )
where

import Control.Monad

import Language.CUTE.Parser.SrcPos

data ParseState
  = ParseState
    { prevLoc :: SrcPos,
      currLoc :: SrcPos }

data ParseResult s a
  = ParseOk s a
  | ParseErr SrcPos SrcPos String

data ParseM a = ParseM { runParseM :: ParseState -> ParseResult ParseState a }

instance Functor ParseM where
  fmap = liftM

instance Applicative ParseM where
  pure = returnParseM
  (<*>) = ap

instance Monad ParseM where
  (>>=) = thenParseM
  fail = failParseM

returnParseM :: a -> ParseM a
returnParseM a =
  ParseM $ \s -> ParseOk s a

thenParseM :: ParseM a -> (a -> ParseM b) -> ParseM b
thenParseM p0 p1 =
  ParseM $ \s0 ->
            case runParseM p0 s0 of
              ParseOk s1 x ->
                runParseM (p1 x) s1
              ParseErr sp0 sp1 msg ->
                ParseErr sp0 sp1 msg

failParseM :: String -> ParseM a
failParseM msg =
  ParseM $ \s -> ParseErr (prevLoc s) (currLoc s) msg

getParseState :: ParseM ParseState
getParseState = ParseM $ \s -> ParseOk s s
