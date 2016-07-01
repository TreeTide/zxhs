import Criterion.Main
import Foreign.Marshal

import ZX.Screen

train :: Sprite8
train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]

alignedTrains, unalignedTrains :: ScreenBits
alignedTrains = trains [xy (8*i) (8*i) | i <- [0..20]]
unalignedTrains = trains [xy (8*i + 3) (8*i + 3) | i <- [0..20]]

trains = foldr (drawSprite train) emptyBits

main :: IO ()
main = do
    buf <- mallocBytes (3*logicalScreenArea)
    let screen = trains [xy 10 10, xy 40 10]
        words = bitsToWords screen
        colors = calcColorTable $
            defaultColors (ColorBlock (Color 3) (Color 7) BrightI)
    defaultMain
        [ bgroup "screenToBytes"
            [ bench "v4" $ whnfIO (screenToBytes4 words colors buf)
            ]
        , bgroup "bitsToWords"
            [ bench "aligned" $ nf bitsToWords alignedTrains
            , bench "unaligned" $ nf bitsToWords unalignedTrains
            ]
        ]
