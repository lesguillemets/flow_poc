{-# LANGUAGE RecordWildCards #-}
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haste.Graphics.AnimationFrame
import Haste.App (MonadIO)

import Data.IORef
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

-- TODO : better impl.
clearCanv :: MonadIO m => Canvas -> m ()
clearCanv = flip render (stroke $ circle (0,0) 0)

sampleDots :: [Dot]
sampleDots = [
             Dot (250,50)  (0,2) (RGBA  20  40 120 0.5) 10,
             Dot (280,90)  (0,2) (RGBA 120  40 120 0.5)  8,
             Dot (280,30)  (0,2) (RGBA 220 220   0 0.5) 12,
             Dot (200,150) (0,2) (RGBA 120  40 120 0.5)  3,
             Dot (120,100) (0,2) (RGBA 120 140 120 0.5) 14,
             Dot (350,100) (0,2) (RGBA 220 240 120 0.5) 15,
             Dot (350,190) (0.2,2) (RGBA 220   0 120 0.5) 15,
             Dot (150,190) (0,2) (RGBA   0 240 120 0.5) 15
             ]

inside :: Double -> Double -> Dot -> Bool
inside w h d = let (x,y) = _loc d
               in
                   0 <= x && x <= w && 0 <= y && y <= h

inside' = inside 500 500

player :: Dot
player = Dot (250,450) (0,0) (RGBA 0 250 100 0.7) 5

main = do
    Just cnv <- getCanvasById "world"
    dots <- newIORef sampleDots
    pressed <- newIORef (S.empty :: S.Set Int)
    _ <- onEvent document KeyDown $ \k ->
            modifyIORef' pressed (S.insert (keyCode k))
    _ <- onEvent document KeyDown $ \k ->
            modifyIORef' pressed (S.delete (keyCode k))
    let mainLoop t0 t1 = do
        let t = t1 - t0
        clearCanv cnv
        renderOnTop cnv . visual $ player
        readIORef dots >>= renderDots cnv
        modifyIORef' dots (filter inside' . map (move (t/100)))
        _ <- requestAnimationFrame (mainLoop t1)
        return ()
    _ <- requestAnimationFrame (mainLoop 0)
    return ()
