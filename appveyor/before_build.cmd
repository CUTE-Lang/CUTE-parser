SET PATH=%PATH%;C:\Users\appveyor\AppData\Roaming\local\bin\

IF "%build%"=="stack" (
   ECHO "STACK/BEFORE_BUILD"
   stack --no-terminal --install-ghc %args% install alex happy
   stack --no-terminal --install-ghc %args% test --bench --only-dependencies
) ELSE (
   ECHO "STYLE/BEFORE_BUILD"
   stack --no-terminal --install-ghc %args% install alex happy hlint
)
