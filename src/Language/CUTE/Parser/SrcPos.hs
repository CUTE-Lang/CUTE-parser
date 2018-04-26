-- |
-- Module      : Language.CUTE.Parser.SrcPos
-- Description :
-- Copyright   : (c) CUTE Lang, 2016-
-- License     : see LICENSE
-- Maintainer  : Junyoung Clare Jang <jjc9310@gmail.com>
-- Portability : Windows, POSIX
--
module Language.CUTE.Parser.SrcPos
  (
    SrcPos(..),
    incSrcPos,
    Posed(..)
  )
where

------------------------------------------------------------
-- Src position type and helper function

data SrcPos
  = SrcPos
    { spFilename :: !String,
      spOffset :: !Int }
    deriving (Eq,Show)

incSrcPos :: Int -> SrcPos -> SrcPos
incSrcPos n (SrcPos f o) = SrcPos f (o + n)

data Posed a
  = Posed SrcPos a
