IF NOT EXIST "stack.exe" (
  curl -ostack.zip -L --insecure http://www.stackage.org/stack/windows-i386
  7z x stack.zip stack.exe
)
