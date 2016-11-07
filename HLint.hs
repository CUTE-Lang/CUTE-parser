-- Hlint hint file

module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Builtin.All


-- Ignore following hints
ignore "Redundant if"
ignore "Use list comprehension"
ignore "Avoid lambda"
-- Response of Compiler, not HLint.
ignore "Parse error"
