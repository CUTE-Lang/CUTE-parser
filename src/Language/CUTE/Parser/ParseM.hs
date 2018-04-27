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
    { strBuf :: StringBuffer,
      prevPos :: SrcPos,
      currPos :: SrcPos }

data ParseResult s a
  = ParseOk s a
  | ParseErr
    { errStartPos :: SrcPos,
      errEndPos :: SrcPos,
      errMsg :: String }

newtype ParseM a
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
thenParseM p fp =
  ParseM $ \s0 ->
            case runParseM p s0 of
              ParseOk s1 x ->
                runParseM (fp x) s1
              ParseErr pos0 pos1 msg ->
                ParseErr pos0 pos1 msg

failParseM :: String -> ParseM a
failParseM msg =
  ParseM $ \s -> ParseErr (prevPos s) (currPos s) msg

failPosParseM :: SrcPos -> SrcPos -> String -> ParseM a
failPosParseM pos0 pos1 msg =
  ParseM $ \_ -> ParseErr pos0 pos1 msg

getParseState :: ParseM ParseState
getParseState = ParseM $ \s -> ParseOk s s

putParseState :: ParseState -> ParseM ()
putParseState s = ParseM $ \_ -> ParseOk s ()

liftMaybe :: String -> Maybe a -> ParseM a
liftMaybe _ (Just a) = returnParseM a
liftMaybe msg Nothing = failParseM msg
