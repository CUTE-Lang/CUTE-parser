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
import Language.CUTE.Parser.Token (TokenCode(..), Token(..))
import Language.CUTE.Parser.LexerHelper
import Language.CUTE.Parser.SrcPos
import Language.CUTE.Parser.ParseM
import Language.CUTE.Parser.StringBuffer
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
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$symbol    = $ascsymbol # [$special \_\"\']

$ascupper = [A-Z]
$upper    = $ascupper

$asclower = [a-z]
$lower    = $asclower

$graphic    = [$lower $upper $decdigit $special \"\']
$printable  = [$graphic $white]

$idstart   = [$lower $upper]
$idchar    = [$lower $upper $decdigit \']

------------------------------------------------------------
-- Alex "Regular expression macro definitions"

@id     = $idstart $idchar*
@sym    = $symbol+

@decimal     = $decdigit+
@binary      = $bindigit+
@octal       = $octdigit+
@hexadecimal = $hexdigit+

@exponent       = [eE] [\-\+]? @decimal
@floating_point = @decimal? \. @decimal @exponent? | @decimal @exponent

@negative_signed = \-
@positive_signed = \+

@single_comment = "//"
@multi_comment = "/*"

------------------------------------------------------------
-- Alex "Identifier"

cute :-

------------------------------------------------------------
-- Alex "Rules"

-- Ignore whitespaces
  $white_not_lf+                        ;

-- Ignore comments
  @single_comment .*                    ;

-- Integers
<0> {
  @decimal                              { tokenInteger positive decimal (0,0) }
  0[bB] @binary                         { tokenInteger positive binary (2,0) }
  0[oO] @octal                          { tokenInteger positive octal (2,0) }
  0[xX] @hexadecimal                    { tokenInteger positive hexadecimal (2,0) }

  @positive_signed @decimal             { tokenInteger positive decimal (1,0) }
  @positive_signed 0[bB] @binary        { tokenInteger positive binary (3,0) }
  @positive_signed 0[oO] @octal         { tokenInteger positive octal (3,0) }
  @positive_signed 0[xX] @hexadecimal   { tokenInteger positive hexadecimal (3,0) }

  @negative_signed @decimal             { tokenInteger negative decimal (1,0) }
  @negative_signed 0[bB] @binary        { tokenInteger negative binary (3,0) }
  @negative_signed 0[oO] @octal         { tokenInteger negative octal (3,0) }
  @negative_signed 0[xX] @hexadecimal   { tokenInteger negative hexadecimal (3,0) }
}

-- Strings
<0> {
  \" [$graphic \ ]* \"                  { tokenString }
}

-- Id and Symbols
<0> {
  @id                                   { tokenByString CTid }
  @sym                                  { tokenByString CTsym }
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

-- Test
<0> {
  "Λ"                                   { token (CTlam Unicode) }
}

------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{
test :: String -> [Token]
test str = tokenize ps
  where
    tokenize :: ParseState -> [Token]
    tokenize (ParseState sb0 pp0 cp0) =
      case alexScan (AlexInput cp0 sb0) 0 of
        AlexEOF -> []
        AlexError ai1 -> error (show ai1)
        AlexSkip (AlexInput cp1 sb1) l ->
          tokenize (ParseState sb1 cp0 cp1)
        AlexToken (AlexInput cp1 sb1) l action ->
          let pm = action cp1 sb0 l
          in
            case runParseM pm (ParseState sb1 cp0 cp1) of
              ParseOk ps2 (Posed _ token) ->
                token : tokenize ps2
              ParseErr esp eep emsg ->
                error ("from " ++ show esp ++ " to " ++ show eep ++
                       ": " ++ emsg)
    ps = ParseState sb sp sp
    sb = stringToStringBuffer str
    sp = SrcPos "test" 0
}
