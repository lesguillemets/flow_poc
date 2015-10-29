{-# LANGUAGE RecordWildCards #-}
module Base where

import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haste.Graphics.AnimationFrame
import Haste.App (MonadIO)

import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Set as S

import Helper

data Dot = Dot {
    _loc :: Point,
    _vel :: Vector,
    _color :: Color,
    _dia :: Double
}

visual :: Dot -> Picture ()
visual d@Dot{..} = color _color . fill $ circle _loc _dia

renderDots :: MonadIO m => Canvas -> [Dot] -> m ()
renderDots cnv = mapM_ (renderOnTop cnv . visual)

move :: Double -> Dot -> Dot
move t d = d{ _loc = _loc d <+> t `smul` (_vel d)}

collides :: Dot -> Dot -> Bool
collides d0 d1 = dist <= (_dia d0 + _dia d1)^(2::Int) where
    (x0,y0) = _loc d0
    (x1,y1) = _loc d1
    dist = (x0-x1)^(2::Int) + (y0-y1)^(2::Int)

inside :: Double -> Double -> Dot -> Bool
inside w h d = let (x,y) = _loc d
               in
                   0 <= x && x <= w && 0 <= y && y <= h
