import Graphics.X11

import qualified Codec.Picture as P (PixelRGB8(..), Pixel8, Image(..)
                                    , writePixel, writeBitmap, GifDelay
                                    , Palette, writeGifImages, palettize
                                    , PaletteOptions(..)
                                    , PaletteCreationMethod(MedianMeanCut)
                                    , GifLooping(LoopingForever)
                                    )
import qualified Codec.Picture.Types as PT (newMutableImage, unsafeFreezeImage)

import Data.Bits ((.&.), shiftR)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Maybe (isJust)

import qualified Data.Sequence as S (Seq, (|>), empty)
import qualified Data.Foldable as F (toList)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad (mapM_, liftM, when, (<=<))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (isEmptyMVar, MVar, newEmptyMVar)

import System.Environment (getArgs)
import System.CPUTime (getCPUTime)

import HSOptions

packPixel :: Image -> (Int, Int) -> P.PixelRGB8
packPixel i (x, y) = let rawPixel = map (fromIntegral . ((flip (.&.)) 255))
                                  $ [shiftR greenShift 8, greenShift, pixel]
                     in P.PixelRGB8 (rawPixel !! 0)
                                    (rawPixel !! 1)
                                    (rawPixel !! 2)

    where pixel = getPixel i (fromIntegral x) (fromIntegral y)
          greenShift = shiftR pixel 8

packImage :: (PrimMonad m) => Int -> Int -> Image -> m (P.Image P.PixelRGB8)
packImage w h i = PT.newMutableImage w h
              >>= \img -> const (PT.unsafeFreezeImage img)
                      =<< mapM_ (\(x, y) -> P.writePixel img x y
                                          . packPixel i $ (x, y))
                          [ (x, h - y - 1) | y <- [0..h-1], x <- [0..w-1] ]

path = "/home/stjepan/test.bmp"

takeScreenshot :: Display -> Window -> Position -> Position
               -> Dimension -> Dimension -> IO (P.Image P.PixelRGB8)
takeScreenshot display rootw x y w h = do

            image   <- getImage display rootw
                                (fromIntegral x) (fromIntegral y)
                                (fromIntegral w) (fromIntegral h)
                                (fromIntegral allPlanes_aux)
                                xyPixmap

            pimage  <- packImage (fromIntegral w) (fromIntegral h)
                                 image

            destroyImage image

            return pimage

getScreenCenter :: Screen -> (Position, Position)
getScreenCenter scr = ((`div` 2) . fromIntegral
                                 . widthOfScreen $ scr,
                       (`div` 2) . fromIntegral
                                 . heightOfScreen $ scr)

mainLoop :: HShotOptions -> IO ()
mainLoop opts = do

    display <- openDisplay ""

    let screen = defaultScreenOfDisplay display

    rootw   <- rootWindow display
                        . screenNumberOfScreen
                        $ screen

    let (w, h) = ( getDim (hs_w opts) (widthOfScreen screen)
                 , getDim (hs_h opts) (heightOfScreen screen)
                 )

    let (c_x, c_y) = getScreenCenter screen

    let (x, y) = ( getPos (hs_x opts) (c_x - (div (fromIntegral w) 2))
                 , getPos (hs_y opts) (c_y - (div (fromIntegral h) 2))
                 )

    runHSOperation (     P.writeBitmap path
                     =<< takeScreenshot display rootw x y w h
                   )
                   (\sec -> either putStrLn id
                        <=< liftM (P.writeGifImages gifPath
                                                    P.LoopingForever)
                          $ record display rootw (hs_delay opts) sec x y w h
                   )
                   $ hs_op opts

    closeDisplay display

gifPath :: FilePath
gifPath = "/home/stjepan/test.gif"

defaultPaletteOptions = P.PaletteOptions
    { P.paletteCreationMethod = P.MedianMeanCut
    , P.enableImageDithering = True
    , P.paletteColorCount = 256
    }

type GifData = (P.Palette, P.GifDelay, P.Image P.Pixel8)

partGifDataToGifData :: P.GifDelay -> (P.Image P.Pixel8, P.Palette) -> GifData
partGifDataToGifData del (img, pal) = (pal, del, img)

record :: Display -> Window -> Int -> Int -> Position -> Position
            -> Dimension -> Dimension -> IO [GifData]
record display rootw del dur x y w h = record' display rootw del (del * 10000)
                                               (dur * 1000000) x y w h S.empty

record' :: Display -> Window -> Int -> Int -> Int -> Position -> Position
        -> Dimension -> Dimension -> S.Seq (GifData) -> IO [GifData]
record' display rootw del delus rem x y w h part
    | rem <= 0  = return $ F.toList part
    | otherwise = do
            start   <- getCPUTime
            shot    <- takeScreenshot display rootw x y w h
            end     <- getCPUTime
            threadDelay . minimum
                        . map abs
                        $ [ 0
                          , delus - fromInteger ((end - start) `div` 1000000)
                          ]
            record' display rootw del delus (rem - delus) x y w h
                  . (S.|>) part
                  . partGifDataToGifData del
                  . P.palettize defaultPaletteOptions
                  $ shot

main :: IO ()
main = maybe invalidUsage (\opt -> bool (mainLoop opt) showHelp (hs_help opt))
   =<< liftM ((flip parseArgs) defOptions) getArgs

    where invalidUsage = putStrLn invalidUsageMsg
                     >>= const (return ())
