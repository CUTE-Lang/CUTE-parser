-- Hlint hint file

module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise
import "hint" HLint.Builtin.All


-- Ignore following hints
ignore "Use list comprehension"
ignore "Use fmap"
-- Response of Compiler, not HLint.
ignore "Parse error"
