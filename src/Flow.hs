{-# LANGUAGE RecordWildCards #-}
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.App (MonadIO)

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

-- TODO : better impl.
clearCanv :: MonadIO m => Canvas -> m ()
clearCanv = flip render (stroke $ circle (0,0) 0)

sampleDots :: [Dot]
sampleDots = [
             Dot (250,50)  (0,0) (RGBA  20  40 120 0.5) 10,
             Dot (280,90)  (0,0) (RGBA 120  40 120 0.5)  8,
             Dot (280,30)  (0,0) (RGBA 220 220   0 0.5) 12,
             Dot (200,150) (0,0) (RGBA 120  40 120 0.5)  3,
             Dot (120,100) (0,0) (RGBA 120 140 120 0.5) 14,
             Dot (350,100) (0,0) (RGBA 220 240 120 0.5) 15
             ]

main = do
    Just cnv <- getCanvasById "world"
    renderDots cnv sampleDots
