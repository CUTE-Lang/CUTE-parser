module Language.CUTE.Parser.Token
  (
    Token(..)
  )
where

------------------------------------------------------------
-- Token Type
--
-- CT stands for CUTE Token
-- Every categories are listed in alphabetic order.
-- \ with integers in single quote (like '\1') in left
-- comments are correspoding parameters of token constructor.
--
-- Comments start at 41th character of that line.
--

data Token

-- CUTE Keywords
  = CTelse                               -- "else"
  | CTif                                 -- "if"
  | CTin                                 -- "in"
  | CTlet                                -- "let"
  | CTthen                               -- "then"

-- CUTE Reserved Symbols
  | CTequal                              -- "="
  | CTlam                                -- "_\"

-- CUTE Special Symbols
  | CTcomma                              -- ","
  | CTcurlybo                            -- "{"
  | CTcurlybc                            -- "}"
  | CTroundbo                            -- "("
  | CTroundbc                            -- ")"
  | CTsemicolon                          -- ";"
  | CTsquarebo                           -- "["
  | CTsquarebc                           -- "]"
  | CTunderscore                         -- "_"

-- CUTE Identifiers
  | CTid String                          -- '\1'

-- CUTE Basic Types
  | CTchar Char                          -- '\1'
                                         -- Character which is surrounded by "'"
  | CTinteger Integer                    -- '\1'
                                         -- Integer with any base
  | CTstring String                      -- '\1'
                                         -- String which is surrounded by "\""

-- CUTE Comments
  | CTcomment String                     -- "!!!"

-- Other Tokens
  | CTeof                                -- end of file
  | CTunknown String                     -- '\1'

-- Typeclass Deriving for Alex
  deriving (Show)

------------------------------------------------------------
