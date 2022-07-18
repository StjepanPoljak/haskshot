import Graphics.X11 (openDisplay, defaultScreenOfDisplay, screenNumberOfScreen
                    , getImage, getPixel, destroyImage, rootWindow, xyPixmap
                    , widthOfScreen, heightOfScreen, Image, allPlanes_aux
                    , closeDisplay)

import Data.Bits ((.&.), shiftR)
import Data.Binary (Word8)

import qualified Data.ByteString as BS (pack, ByteString, empty)
import Codec.BMP (packRGBA32ToBMP, writeBMP)

import qualified Data.Sequence as S (Seq, (><), fromList, empty)
import qualified Data.Foldable as F (toList)

packPixel :: Image -> (Int, Int) -> S.Seq Word8
packPixel image (x, y) = S.fromList
                       . map (fromIntegral . ((flip (.&.)) 255))
                       $ [shiftR greenShift 8, greenShift, pixel, 0]

    where pixel = getPixel image (fromIntegral x) (fromIntegral y)
          greenShift = shiftR pixel 8

packImage :: Int -> Int -> Image -> BS.ByteString
packImage w h i = BS.pack
                . F.toList
                . foldl (\acc (x, y) -> acc S.>< packPixel i (x, y))
                        S.empty
                $ [ (x, h - y) | y <- [0..h-1], x <- [0..w-1] ]

path = "/home/stjepan/test.bmp"

main :: IO ()
main = do

    display <- openDisplay ""

    let screen = defaultScreenOfDisplay display
    let scrnum = screenNumberOfScreen screen

    rootw <- rootWindow display scrnum

    let screenWidth = widthOfScreen screen
    let screenHeight = heightOfScreen screen

    image <- getImage display rootw 0 0 (fromIntegral screenWidth)
                                        (fromIntegral screenHeight)
                      (fromIntegral allPlanes_aux) xyPixmap

    writeBMP path . packRGBA32ToBMP (fromIntegral screenWidth)
                                    (fromIntegral screenHeight)
                  . packImage (fromIntegral screenWidth)
                              (fromIntegral screenHeight)
                  $ image

    destroyImage image
    closeDisplay display

    return ()
