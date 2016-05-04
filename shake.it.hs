{-# LANGUAGE UnicodeSyntax #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  "clean" ∫ cabal ["clean"]

  buildPath </> "flipper.exe" ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "force" ∫ do
    cabal ["install", "--force-reinstalls", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "install" ◉ [buildPath </> "flipper.exe"] ∰
    cabal ["install"]

  "test" ◉ [buildPath </> "flipper.exe"] ∰ do
    currentPath ← getCurrentDirectory
    setCurrentDirectory $ currentPath </> buildPath
    let flipper a =
          system ( "chcp 65001 && "
                 ++ "flipper.exe"
                 ++ " " ++ a)
            >>= checkExitCode
    removeIfExists "2.jpg"
    flipper "1.jpg 2.jpg"

 where buildPath :: String
       buildPath = "dist/build/flipper"
