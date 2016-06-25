import Criterion.Main
import Foreign.Marshal

import ZX.Screen

train :: Sprite8
train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]

main :: IO ()
main = do
    buf <- mallocBytes (3*logicalScreenArea)
    let screen = foldr (drawSprite train) emptyBits [xy 10 10, xy 40 10]
        words = bitsToWords screen
        colors = defaultColors (ColorBlock (Color 3) (Color 7) BrightI)
    defaultMain
        [ bgroup "screenToBytes"
            [ bench "v4" $ whnfIO (screenToBytes4 words colors buf)
            , bench "v5" $ whnfIO (screenToBytes5 words colors buf)
            , bench "v3" $ whnfIO (screenToBytes3 screen colors buf)
            ]
        ]
