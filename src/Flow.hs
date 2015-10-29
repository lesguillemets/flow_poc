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

import Base
import Helper
import KeyHandler


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

inside' = inside 500 500

player :: Dot
player = Dot (250,450) (0,0) (RGBA 0 250 100 0.7) 5

maxPlayerSpeed :: Double
maxPlayerSpeed = 30
accel' = accel maxPlayerSpeed

main = do
    Just cnv <- getCanvasById "world"
    dots <- newIORef sampleDots
    pressed <- newIORef (S.empty :: S.Set Int)
    pl <- newIORef player
    _ <- addKeyHandler pressed
    let mainLoop t0 t1 = do
        let t = t1 - t0
        clearCanv cnv
        readIORef pl >>= renderOnTop cnv . visual
        readIORef dots >>= renderDots cnv
        modifyIORef' dots (filter inside' . map (move (t/100)))
        a <- fromPressed <$> readIORef pressed
        if (a /= (0,0))
            then modifyIORef' pl (move (t/100) . accel' ((t/100) `smul` a))
            else modifyIORef' pl (move (t/100) . slow 10)
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
