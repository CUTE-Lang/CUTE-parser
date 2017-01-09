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
    putParseState,
    liftMaybe,
  )
where

import Control.Monad
import Control.Monad.Fail

import Language.CUTE.Parser.SrcPos
import Language.CUTE.Parser.StringBuffer


data ParseState
  = ParseState
    { stringBuffer :: StringBuffer,
      prevPos :: SrcPos,
      currPos :: SrcPos }

data ParseResult s a
  = ParseOk s a
  | ParseErr
    { errStartPos :: SrcPos,
      errEndPos :: SrcPos,
      errMsg :: String }

data ParseM a
  = ParseM
    { runParseM :: ParseState -> ParseResult ParseState a }

instance Functor ParseM where
  fmap = liftM

instance Applicative ParseM where
  pure = returnParseM
  (<*>) = ap

instance Monad ParseM where
  (>>=) = thenParseM
  fail = failParseM

instance MonadFail ParseM where
  fail = failParseM

returnParseM :: a -> ParseM a
returnParseM a =
  a `seq` ParseM $ \s -> ParseOk s a

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
  ParseM $ \s -> ParseErr (prevPos s) (currPos s) msg

failPosParseM :: SrcPos -> SrcPos -> String -> ParseM a
failPosParseM loc0 loc1 msg =
  ParseM $ \s -> ParseErr loc0 loc1 msg

getParseState :: ParseM ParseState
getParseState = ParseM $ \s -> ParseOk s s

putParseState :: ParseState -> ParseM ()
putParseState s1 = ParseM $ \s0 -> ParseOk s1 ()

liftMaybe :: String -> Maybe a -> ParseM a
liftMaybe _ (Just a) = returnParseM a
liftMaybe msg Nothing = failParseM msg
