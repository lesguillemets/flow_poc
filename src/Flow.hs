{-# LANGUAGE RecordWildCards #-}
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

fromPressed :: S.Set Int -> Vector
fromPressed = foldl' (<+>) (0,0) . map keyConfig
        . S.toList . S.filter (`elem` [65,87,68,83, 37,38,39,40])

baseSpeed :: Double
baseSpeed = 10

keyConfig :: Int -> Vector
keyConfig 65 = (-baseSpeed,0)
keyConfig 87 = (0,-baseSpeed)
keyConfig 68 = (baseSpeed,0)
keyConfig 83 = (0,baseSpeed)
keyConfig 37 = (-baseSpeed,0)
keyConfig 38 = (0,-baseSpeed)
keyConfig 39 = (baseSpeed,0)
keyConfig 40 = (0,baseSpeed)
keyConfig _ = (0,0)

main = do
    Just cnv <- getCanvasById "world"
    dots <- newIORef sampleDots
    pressed <- newIORef (S.empty :: S.Set Int)
    pl <- newIORef player
    _ <- onEvent document KeyDown $ \k ->
            modifyIORef' pressed (S.insert (keyCode k))
    _ <- onEvent document KeyUp $ \k ->
            modifyIORef' pressed (S.delete (keyCode k))
    let mainLoop t0 t1 = do
        let t = t1 - t0
        clearCanv cnv
        readIORef pl >>= renderOnTop cnv . visual
        readIORef dots >>= renderDots cnv
        modifyIORef' dots (filter inside' . map (move (t/100)))
        v <- fromPressed <$> readIORef pressed
        modifyIORef' pl (move (t/100) . \d -> d{_vel = v})
        checkCollision pl dots
        _ <- requestAnimationFrame (mainLoop t1)
        return ()
    _ <- requestAnimationFrame (mainLoop 0)
    return ()

checkCollision :: IORef Dot -> IORef [Dot] -> IO ()
checkCollision p ds = do
    pl <- readIORef p
    dots <- readIORef ds
    when (any (collides pl) dots) $ do
        writeLog "ouch"
