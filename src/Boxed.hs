{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-imports
  #-}

{-# LANGUAGE
    UnicodeSyntax
  , ScopedTypeVariables
  , FlexibleContexts
  , TypeFamilies
  #-}

module Boxed
  ( BoxedImage(..)
  , readImage
  , writeJpg
  , writePng
  , Pixel -- WordX
  ) where

import  Data.List (sort)
import  Data.Maybe (fromMaybe, maybeToList)
import  Data.Word

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as VG

import qualified Codec.Picture          as Juicy
import qualified Codec.Picture.Types    as Juicy.Types
import qualified Codec.Picture.Jpg      as Juicy.Jpg
import qualified Codec.Picture.Metadata as Met

{- We want to be able to store any kind of pixel value, not just `Storable` values,
   so we declare our own `BoxedImage`. We will simply store pixels in row-major
   order in a boxed `Vector`. -}
data BoxedImage a = BoxedImage
  { biWidth  :: !Int
  , biHeight :: !Int
  , biData   :: !(V.Vector a)
  }

instance Functor BoxedImage where
  fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

{- Now, we want to be able to convert from a JuicyPixels image to our own
   `BoxedImage` and back again. In this blogpost, we will only deal with grayscale
   images (`BoxedImage Word8`), since this makes the image processing algorithms
   mentioned here a lot easier to understand. -}

type Pixel = Word8
  --Word8  -- Grayscale

boxPixel8 :: Juicy.Image Juicy.Types.Pixel8 → BoxedImage Pixel
boxPixel8 image = BoxedImage
  { biWidth  = Juicy.imageWidth image
  , biHeight = Juicy.imageHeight image
  , biData   = VG.convert (Juicy.imageData image)
  }
boxYCbCr8 :: Juicy.Image Juicy.Types.PixelYCbCr8 → BoxedImage Pixel
boxYCbCr8 image = BoxedImage
  { biWidth  = Juicy.imageWidth image
  , biHeight = Juicy.imageHeight image
  , biData   = VG.convert (Juicy.imageData image)
  }

unboxPng :: BoxedImage Pixel → Juicy.Image Juicy.Pixel8
unboxPng boxedImage = Juicy.Image
    { Juicy.imageWidth  = biWidth boxedImage
    , Juicy.imageHeight = biHeight boxedImage
    , Juicy.imageData   = VG.convert (biData boxedImage)
    }
unboxYCbCr8 :: BoxedImage Pixel → Juicy.Image Juicy.Types.PixelYCbCr8
unboxYCbCr8 boxedImage = Juicy.Image
    { Juicy.imageWidth  = biWidth boxedImage
    , Juicy.imageHeight = biHeight boxedImage
    , Juicy.imageData   = VG.convert (biData boxedImage)
    }

{- With the help of `boxImage` and `unboxImage`, we can now call out to the
   JuicyPixels library: -}
readImage :: FilePath → IO (BoxedImage Pixel)
readImage filePath = do
  errOrImage ← Juicy.readImageWithMetadata filePath
  case errOrImage of
    Right (Juicy.ImageY8 img, _) → return (boxPixel8 img)
    Right (Juicy.ImageYCbCr8 img, _) → return (boxYCbCr8 img)
    Right _ → error "readImage: unsupported format"
    Left err → error $ "readImage: could not load image: " ++ err

writePng :: FilePath → BoxedImage Pixel → IO ()
writePng filePath = Juicy.writePng filePath . unboxPng

writeJpg :: FilePath → BoxedImage Pixel → IO ()
writeJpg filePath boxed = do
  --let metas = Met.insert Met.Author "flipper"
  --          $ Met.mkDpiMetadata 93
  let img = unboxYCbCr8 boxed
      i = Juicy.Types.convertImage img :: Juicy.Types.Image Juicy.Types.PixelYCbCr8
      --m = Juicy.Jpg.encodeJpegAtQualityWithMetadata 100 metas i
      m = Juicy.Jpg.encodeJpegAtQuality 90 i
  L.writeFile filePath m
