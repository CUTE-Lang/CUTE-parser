IF "%build%"=="stack" (
   ECHO "STACK/BUILD"
   stack --no-terminal %args% test --bench --no-run-benchmarks --haddock --no-haddock-deps
) ELSE (
   ECHO "STYLE/BUILD"
   hlint src/ test/ --cpp-simple --hint=HLint.hs
   stack --no-terminal build --ghc-options -Wall
)
