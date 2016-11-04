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
module Language.CUTE.Parser.Lexer
  (
    test
  )
where

-- Imported modules
import Language.CUTE.Parser.Token (Token(..))
import Language.CUTE.Parser.LexerHelper
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

$graphic    = [$lower $upper $decdigit $special \"\']

$idchar    = [$lower $upper $decdigit \']

------------------------------------------------------------
-- Alex "Regular expression macro definitions"

@id     = $idchar+
@sym    = $symbol+

@decimal     = $decdigit+
@binary      = $bindigit+
@octal       = $octdigit+
@hexadecimal = $hexdigit+

@exponent       = [eE] [\-\+]? @decimal
@floating_point = @decimal? \. @decimal @exponent? | @decimal @exponent

@negative = \-
@positive = \+?

------------------------------------------------------------
-- Alex "Identifier"

cute :-

------------------------------------------------------------
-- Alex "Rules"

-- Ignore whitespaces
  $white_not_lf+                        ;

-- Integers
<0> {
  @positive @decimal                    { tokenInteger positive decimal (0,0) }
  @negative @decimal                    { tokenInteger negative decimal (1,0) }
  @positive 0[bB] @binary               { tokenInteger positive binary (2,0) }
  @negative 0[bB] @binary               { tokenInteger negative binary (3,0) }
  @positive 0[oO] @octal                { tokenInteger positive octal (2,0) }
  @negative 0[oO] @octal                { tokenInteger negative octal (3,0) }
  @positive 0[xX] @hexadecimal          { tokenInteger positive hexadecimal (2,0) }
  @negative 0[xX] @hexadecimal          { tokenInteger negative hexadecimal (3,0) }
}

-- Strings
<0> {
  \" [$graphic \ ]* \"                  { tokenString }
}

-- Special symbols
<0> {
  "("                                   { token CTroundbo }
  ")"                                   { token CTroundbc }
  "{"                                   { token CTcurlybo }
  "}"                                   { token CTcurlybc }
  "["                                   { token CTsquarebo }
  "]"                                   { token CTsquarebc }
}

------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{
test :: String -> [Token]
test s = tokenize ai
  where
    tokenize :: AlexInput -> [Token]
    tokenize ai =
      case alexScan ai 0 of
        AlexEOF -> []
        AlexError ai' -> error (show ai')
        AlexSkip ai' l -> tokenize ai'
        AlexToken ai' l action ->
          let AlexInput sp pc bs cs = ai
              (spt, token) = action sp l (take l cs)
          in token : tokenize ai'
    ai = AlexInput sp '$' [] s
    sp = SrcPos "test" 0
}
