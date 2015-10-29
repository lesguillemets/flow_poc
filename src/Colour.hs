module Data.Colour (
                   fromHSV
                   )where
import Haste.Graphics.Canvas (Color(..))

type HSV = (Double, Double, Double)
-- [0,360] \times [0,1] \times [0,1]

-- from: https://en.wikipedia.org/wiki/HSL_and_HSV#Converting_to_RGB
fromHSV :: HSV -> Color
fromHSV (_,0,v) = toC (v,v,v)
fromHSV (_,_,0) = toC (0,0,0)
fromHSV (h,s,v) =
        toC $ case hi of
            0 -> (v,x,m)
            1 -> (x,v,m)
            2 -> (m,v,x)
            3 -> (m,x,v)
            4 -> (x,m,v)
            _ -> (v,m,x)
            where
                hi = floor (h/60) :: Int
                f = h/60 - fromIntegral hi
                m = v * (1-s)
                x = if hi `mod` 2 == 0
                        then v * (1- (1-f)*s)
                        else v * (1- f*s)

toC :: RealFrac a => (a, a, a) -> Color
toC (r,g,b) = RGB (fr r) (fr g) (fr b) where
    fr = floor . (*255)
