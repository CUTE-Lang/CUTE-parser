-- |
-- Module      : Language.CUTE.Parser.SrcPos
-- Description :
-- Copyright   : (c) CUTE Lang, 2016-
-- License     : see LICENSE
-- Maintainer  : Junyoung Clare Jang <jjc9310@gmail.com>
--             , Youngmin Cho <>
--             , Na Yeon Park <>
-- Portability : Windows, POSIX
--
module Language.CUTE.Parser.SrcPos
  (
    SrcPos(..),
    increaseSrcPos
  )
where

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
