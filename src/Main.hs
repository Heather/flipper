{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-imports
  #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

import           System.Environment

import           Data.List              (sort)
import           Data.Maybe             (fromMaybe, maybeToList)

import qualified Data.ByteString.Lazy   as L
import qualified Data.Vector            as V
import qualified Data.Vector.Generic    as VG

import qualified Codec.Picture          as Juicy
import qualified Codec.Picture.Jpg      as Juicy.Jpg
import qualified Codec.Picture.Metadata as Met
import qualified Codec.Picture.Types    as Juicy.Types

import           Focused
import           Median

main ∷ IO ()
main = do
  [input, output] ← getArgs
  image ← readImage input -- $ extend blur
  let i = unfocus $ focus image
  writePng (output ++ ".png") i
  writeJpg output i
  putStrLn "Success"
