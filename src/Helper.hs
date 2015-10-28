module Helper (
    smul
  , vadd , (<+>)
              ) where

import Haste.Graphics.Canvas

smul :: Double -> Vector -> Vector
smul a (x,y) = (a*x,a*y)

vadd :: Vector -> Vector -> Vector
vadd (x,y) (z,w) = (x+z, y+w)

(<+>) :: Vector -> Vector -> Vector
(<+>) = vadd
infixl 6 <+>
