module Spawn (spawn, spawnN, defaultSpConfig, upSpConfig) where

import Haste

import Base
import Helper
import Colour

data SpawnConfig = SpConfig {
    _w :: Double,
    _h :: Double
}

defaultSpConfig = SpConfig 500 250
upSpConfig = SpConfig 500 10

-- TODO : monadic
spawn :: SpawnConfig -> Seed -> (Dot, Seed)
spawn (SpConfig w h) s = let
    (wr, s') = randomR (0,w) s
    (hr, s'') = randomR (0,h) s'
    (hue, s''') = randomR (0,360) s''
    (vdx, s'''') = randomR (-2,2) s'''
    (vdy, s''''') = randomR (-2,2) s''''
    (di, s'''''') = randomR (3,15) s'''''
    in
        (
        Dot (wr,hr)
            ((0,5) <+> (vdx,vdy))
            ((`withAlpha` 0.5) . fromHSV $ (hue,0.8,1))
            di
        ,
        s'''''')

spawnN :: SpawnConfig -> Seed -> Int -> ([Dot],Seed)
spawnN conf s n = sn [] s n where
    sn ds se 0 = (ds,se)
    sn ds se k = let (d,s') = spawn conf se
                       in
                           sn (d:ds) s' (k-1)
