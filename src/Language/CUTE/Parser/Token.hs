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

data TokenCode
  = Ascii
  | Unicode

-- Typeclass Deriving for Token Type
  deriving (Show)

data Token

-- CUTE Keywords
  = CTelse                               -- "else"
  | CTif                                 -- "if"
  | CTin                                 -- "in"
  | CTlet                                -- "let"
  | CTthen                               -- "then"

-- CUTE Reserved Symbols
  | CTequal                              -- "="
  | CTlam TokenCode                      -- "_\" for ascii
                                         -- "λ" for unicode

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

-- CUTE Identifying Strings
  | CTid String                          -- '\1'
                                         -- Identifier for variables or contructors
  | CTsym String                         -- '\1'
                                         -- Symbols for variable operators or
                                         -- constructor operators

-- CUTE Basic Types
  | CTbool Bool                          -- '\1'
                                         -- True or False
  | CTchar Char                          -- '\1'
                                         -- Character which is surrounded by "'"
  | CTinteger Integer                    -- '\1'
                                         -- Integer with any base
  | CTstring String                      -- \"'\1'\"
                                         -- String which is surrounded by "\""

-- CUTE Comments
  | CTcomment String                     -- "!!!"

-- Other Tokens
  | CTeof                                -- End of file
  | CTunknown String                     -- '\1'

-- Typeclass Deriving for Alex
  deriving (Show)

------------------------------------------------------------
