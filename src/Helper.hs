module Helper (
    smul
  , vadd , (<+>)
  , withAlpha
              ) where

import Haste.Graphics.Canvas

smul :: Double -> Vector -> Vector
smul a (x,y) = (a*x,a*y)

vadd :: Vector -> Vector -> Vector
vadd (x,y) (z,w) = (x+z, y+w)

(<+>) :: Vector -> Vector -> Vector
(<+>) = vadd
infixl 6 <+>

withAlpha :: Color -> Double -> Color
withAlpha c@(RGBA{}) _ = c
withAlpha (RGB r g b) a = RGBA r g b a
