{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-imports
  #-}

{-# LANGUAGE
    UnicodeSyntax
  , ScopedTypeVariables
  , FlexibleContexts
  , TypeFamilies
  , BangPatterns
  #-}

{- taken from example just for test -}

module Median
  ( median
  , reduceNoise1
  , blur
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

import Control.Applicative ((<$>))

import Focused

reduceNoise1 :: FocusedImage Pixel → Pixel
reduceNoise1 pixel = median
  [ extract p
  | x ← [-2, -1 .. 2], y ← [-2, -1 .. 2]
  , p ← maybeToList (neighbour x y pixel)
  ]

median :: Integral a => [a] → a
median xs
  | odd len   = sort xs !! (len `div` 2)
  | otherwise = case drop (len `div` 2 - 1) (sort xs) of
          (x : y : _) → x `div` 2 + y `div` 2
          _           → error "median: empty list"
 where !len = length xs

blur :: FocusedImage Pixel → Pixel
blur pixel = fromMaybe (extract pixel) $ do
  let self = fromIntegral (extract pixel) :: Int
  topLeft     ← extractNeighbour (-1) (-1)
  top         ← extractNeighbour   0  (-1)
  topRight    ← extractNeighbour   1  (-1)
  right       ← extractNeighbour   1    0
  bottomRight ← extractNeighbour   1    1
  bottom      ← extractNeighbour   0    1
  bottomLeft  ← extractNeighbour (-1)   1
  left        ← extractNeighbour (-1)   0
  return $ fromIntegral $ (`div` 16) $
    self * 4 +
    top * 2 + right * 2 + bottom * 2 + left * 2 +
    topLeft + topRight + bottomRight + bottomLeft
 where
   extractNeighbour :: Int → Int → Maybe Int
   extractNeighbour x y = fromIntegral . extract <$> neighbour x y pixel
