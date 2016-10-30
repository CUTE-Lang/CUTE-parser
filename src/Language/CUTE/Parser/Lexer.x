------------------------------------------------------------
-- (c) Junyoung Clare Jang, Youngmin Cho, Na Yeon Park
--
-- Lexer for CUTE
--
-- This lexer is made up with regular grammar for Alex lexer
-- generator [1] and some hard coded Haskell codes.
--
-- Ref/
-- [1]: http://www.haskell.org/alex/
--
------------------------------------------------------------

-- Developer Comments
-- TODO:
--
--
-- Known Bugs:
--
--

------------------------------------------------------------
-- Alex "Haskell code fragment top"

{
-- Add warnings in code generation of Alex
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- Module definition
module Lexer
  (
  )
where

-- Imported modules
import Language.CUTE.Parser.Token (Token(..))
}

------------------------------------------------------------
-- -- Alex "Wrapper"

-- %wrapper "basic"

------------------------------------------------------------
-- Alex "Character set macro definitions"

$lf           = [\n\r\f]
$white        = [$lf\v\ ]
$white_not_lf = $white # $lf
$tab          = \t

$ascdigit = 0-9
$decdigit = $ascdigit
$digit    = $ascdigit

$bindigit  = 0-1
$octdigit  = 0-7
$hexdigit  = [$decdigit A-F a-f]

-- Check specials to match it with CUTE syntax
$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\.\/\<\=\>\?\@\\\^\|\-\~\:]
$symbol    = $ascsymbol # [$special \_\"\']

$ascupper = [A-Z]
$upper    = $ascupper

$asclower = [a-z]
$lower    = $asclower

$graphic    = [$lower $upper $digit $special \"\']

$idchar    = [$lower $upper $digit \']

------------------------------------------------------------
-- Alex "Regular expression macro definitions"

------------------------------------------------------------
-- Alex "Identifier"

cute :-


------------------------------------------------------------
-- Alex "Rules"

------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{
}
