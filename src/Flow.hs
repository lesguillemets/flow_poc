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
import Spawn


-- TODO : better impl.
clearCanv :: MonadIO m => Canvas -> m ()
clearCanv = flip render (stroke $ circle (0,0) 0)

inside' = inside 500 500

player :: Dot
player = Dot (250,450) (0,0) normalColor 5

normalColor :: Color
normalColor = RGBA 0 250 100 0.7
hitColor :: Color
hitColor = RGBA 230 150 100 0.7

maxPlayerSpeed :: Double
maxPlayerSpeed = 30
accel' = accel maxPlayerSpeed

main = do
    Just cnv <- getCanvasById "world"
    se <- newSeed
    let (initDots, s') = spawnN defaultSpConfig se 50
    dots <- newIORef initDots
    pressed <- newIORef (S.empty :: S.Set Int)
    pl <- newIORef player
    seed <- newIORef s'
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
            else modifyIORef' pl (move (t/100) . slow 0.9)
        replenishDots 50 dots seed
        checkCollision pl dots
        _ <- requestAnimationFrame (mainLoop t1)
        return ()
    _ <- requestAnimationFrame (mainLoop 0)
    return ()

replenishDots n d s = do
    se <- readIORef s
    dots <- readIORef d
    writeLog "HI"
    let
        num = max 0 $ n - length dots
        (newDots,s') = spawnN upSpConfig se num
    modifyIORef' d (newDots ++)
    modifyIORef' s (const s')
    writeLog "HI"


checkCollision :: IORef Dot -> IORef [Dot] -> IO ()
checkCollision p ds = do
    pl <- readIORef p
    dots <- readIORef ds
    if any (collides pl) dots
        then do
            writeLog "ouch"
            modifyIORef' p (\d -> d{_color = hitColor})
        else
            modifyIORef' p (\d -> d{_color = normalColor})
