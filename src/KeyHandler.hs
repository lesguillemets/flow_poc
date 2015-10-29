module KeyHandler where

import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas (Vector)

import Data.IORef
import Data.List
import qualified Data.Set as S

import Helper

fromPressed :: S.Set Int -> Vector
fromPressed = foldl' (<+>) (0,0) . map keyConfig
        . S.toList . S.filter (`elem` [65,87,68,83, 37,38,39,40])

keyConfig :: Int -> Vector
keyConfig 65 = (-baseAccel,0)
keyConfig 87 = (0,-baseAccel)
keyConfig 68 = (baseAccel,0)
keyConfig 83 = (0,baseAccel)
keyConfig 37 = (-baseAccel,0)
keyConfig 38 = (0,-baseAccel)
keyConfig 39 = (baseAccel,0)
keyConfig 40 = (0,baseAccel)
keyConfig _ = (0,0)

baseAccel :: Double
baseAccel = 5

addKeyHandler :: IORef (S.Set Int) -> IO (HandlerInfo, HandlerInfo)
addKeyHandler pressed = do
    d <- onEvent document KeyDown $ \k ->
            modifyIORef' pressed (S.insert (keyCode k))
    u <- onEvent document KeyUp $ \k ->
            modifyIORef' pressed (S.delete (keyCode k))
    return (d,u)
