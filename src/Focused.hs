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

module Focused
  ( FocusedImage(..)
  , focus
  , unfocus
  , extend
  , extract
  , neighbour
  , module Boxed
  ) where

import  Data.List (sort)
import  Data.Maybe (fromMaybe, maybeToList)
import  Data.Word (Word8)

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as VG

import qualified Codec.Picture          as Juicy
import qualified Codec.Picture.Types    as Juicy.Types
import qualified Codec.Picture.Jpg      as Juicy.Jpg
import qualified Codec.Picture.Metadata as Met

import Boxed

{- additional type that focuses on a specific
   location in the image. We typically want to use a smart constructor for this, so
   that we don't allow focusing on an `(x, y)`-pair outside of the `piBoxedImage`. -}
data FocusedImage a = FocusedImage
  { piBoxedImage :: !(BoxedImage a)
  , piX          :: !Int
  , piY          :: !Int
  }

-- Conversion to and from a `BoxedImage` is easy:
focus :: BoxedImage a → FocusedImage a
focus bi
  | biWidth bi > 0 && biHeight bi > 0 = FocusedImage bi 0 0
  | otherwise                         = error "Cannot focus on empty images"

unfocus :: FocusedImage a → BoxedImage a
unfocus (FocusedImage bi _ _) = bi

-- And the functor instance is straightforward, too:
instance Functor FocusedImage where
  fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y

-- Now, we can implement the fabled Comonad class:
class Functor w => Comonad w where
  extract :: w a → a
  extend  :: (w a → b) → w a → w b

{- We want to convert all pixels in the image, and the conversion function is supplied as
   f :: FocusedImage a -> b. In order to apply this to all pixels in the image,
   we must thus create a FocusedImage for every position in the image. Then, we can simply pass
   this to f which gives us the result at that position. -}
instance Comonad FocusedImage where
  extract (FocusedImage bi x y) =
    biData bi V.! (y * biWidth bi + x)

  extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
    (BoxedImage w h $ V.generate (w * h) $ \i →
      let (y', x') = i `divMod` w
      in f (FocusedImage bi x' y'))
    x y

{- We're almost done with our mini-framework. One thing that remains is that we
   want to be able to look around in a pixel's neighbourhood easily. In order to do
   this, we create this function which shifts the focus by a given pair of
   coordinates: -}
neighbour :: Int → Int → FocusedImage a → Maybe (FocusedImage a)
neighbour dx dy (FocusedImage bi x y)
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage bi x' y')
 where
   x'          = x + dx
   y'          = y + dy
   outOfBounds =
     x' < 0 || x' >= biWidth bi ||
     y' < 0 || y' >= biHeight bi
