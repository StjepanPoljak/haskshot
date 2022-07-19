import Graphics.X11 (openDisplay, defaultScreenOfDisplay, screenNumberOfScreen
                    , getImage, getPixel, destroyImage, rootWindow, xyPixmap
                    , widthOfScreen, heightOfScreen, Image, allPlanes_aux
                    , closeDisplay, Display, Screen, Position, Dimension
                    )

import qualified Codec.Picture as P (PixelRGB8(..), Pixel8, Image (..)
                                    , DynamicImage(..), writePixel
                                    , writeDynamicBitmap
                                    )
import qualified Codec.Picture.Types as PT (newMutableImage, unsafeFreezeImage)

import Data.Bits ((.&.), shiftR)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad (mapM_, liftM, void)

packPixel :: Image -> (Int, Int) -> P.PixelRGB8
packPixel i (x, y) = let rawPixel = map (fromIntegral . ((flip (.&.)) 255))
                                  $ [shiftR greenShift 8, greenShift, pixel]
                     in P.PixelRGB8 (rawPixel !! 0)
                                    (rawPixel !! 1)
                                    (rawPixel !! 2)

    where pixel = getPixel i (fromIntegral x) (fromIntegral y)
          greenShift = shiftR pixel 8

packImage :: (PrimMonad m) => Int -> Int -> Image -> m P.DynamicImage
packImage w h i = liftM P.ImageRGB8 $ PT.newMutableImage w h
              >>= \img -> const (PT.unsafeFreezeImage img)
                      =<< mapM_ (\(x, y) -> P.writePixel img x y
                                          . packPixel i $ (x, y))
                          [ (x, h - y - 1) | y <- [0..h-1], x <- [0..w-1] ]

path = "/home/stjepan/test.bmp"

takePartScreenshot :: Display -> Screen -> Position -> Position
                   -> Dimension -> Dimension -> IO P.DynamicImage
takePartScreenshot display screen x y w h = do

            rootw   <- rootWindow display
                     . screenNumberOfScreen
                     $ screen

            image   <- getImage display rootw
                                (fromIntegral x) (fromIntegral y)
                                (fromIntegral w) (fromIntegral h)
                                (fromIntegral allPlanes_aux)
                                xyPixmap

            pimage  <- packImage (fromIntegral w) (fromIntegral h)
                                 image

            destroyImage image

            return pimage

takeScreenshot :: Display -> Screen -> IO P.DynamicImage
takeScreenshot display screen = let w = widthOfScreen screen
                                    h = heightOfScreen screen
                                in takePartScreenshot display screen 0 0 w h

main :: IO ()
main = do

    display <- openDisplay ""

    let screen = defaultScreenOfDisplay display

    P.writeDynamicBitmap path =<< takeScreenshot display screen

    closeDisplay display

    return ()
