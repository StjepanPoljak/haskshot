module HSOptions ( getDim
                 , getPos
                 , parseArgs
                 , runHSOperation
                 , showHelp
                 , defOptions
                 , invalidUsageMsg
                 , HShotOptions(..)
                 ) where

import Graphics.X11 (Position, Dimension)

import Data.Bool (bool)
import qualified Data.Map as M (Map, fromList, (!?))

import Control.Monad (liftM, mfilter)

readsMaybe :: (Read a) => String -> Maybe a
readsMaybe x = case reads x of
        [(res, [])]     -> Just res
        _               -> Nothing

data HSPosition = HSCoord Position
                | HSCenter
                deriving (Eq, Show)

instance Read HSPosition where
    readsPrec _ = maybe [] (\d -> [(d, "")]) . readHSPosition

readHSPosition :: String -> Maybe HSPosition
readHSPosition str = bool (return . HSCoord =<< readsMaybe str)
                          (Just HSCenter) (str == "c")

getPos :: HSPosition -> Position -> Position
getPos HSCenter alt = alt
getPos (HSCoord x) _ = x

data HSDimension = HSSize Dimension
                 | HSSpan
                 deriving (Eq, Show)

instance Read HSDimension where
    readsPrec _ = maybe [] (\d -> [(d, "")]) . readHSDimension

getDim :: HSDimension -> Dimension -> Dimension
getDim HSSpan alt = alt
getDim (HSSize x) _ = x

readHSDimension :: String -> Maybe HSDimension
readHSDimension str = bool (return . HSSize =<< readsMaybe str)
                           (Just HSSpan) (str == "s")
data HSOperation = HSScreenshot
                 | HSRecord Int
                 | HSRecLoop
                 deriving (Eq, Show)

instance Read HSOperation where
    readsPrec _ = maybe [] (\d -> [(HSRecord d, "")]) . readsMaybe

runHSOperation :: a -> (Int -> a) -> HSOperation -> a
runHSOperation scrs _ HSScreenshot = scrs
runHSOperation _ rec (HSRecord time) = rec time

data HShotOptions = HShotOptions { hs_x     :: HSPosition
                                 , hs_y     :: HSPosition
                                 , hs_w     :: HSDimension
                                 , hs_h     :: HSDimension
                                 , hs_op    :: HSOperation
                                 , hs_delay :: Int
                                 , hs_help  :: Bool
                                 } deriving Show

defOptions :: HShotOptions
defOptions = HShotOptions (HSCoord 0) (HSCoord 0) HSSpan HSSpan
                          HSScreenshot 50 False

data HSOptMode = HSOption
               | HSSwitch
               deriving Eq

runOptMode :: a -> a -> HSOptMode -> a
runOptMode opt _ HSOption = opt
runOptMode _ switch HSSwitch = switch

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) lst no
    | no `elem` [0..len-1]  = Just . (!!) lst $ no
    | otherwise             = Nothing
    where len = length lst

parseArgs :: [String] -> HShotOptions -> Maybe HShotOptions
parseArgs [] part = Just part
parseArgs (x:xs) part = (mfilter (=='-') . head' . take 1) x
                    >>= const ((!?) x 1)
                    >>= (M.!?) optMap
                    >>= \tpl -> parseArgs (runOptMode (tail' xs) xs
                                                      (third tpl))
                            =<< (first tpl) part
                            =<< runOptMode (head' xs) (Just "")
                                           (third tpl)
    where tail' [] = []
          tail' (x:xs) = xs
          head' [] = Nothing
          head' (x:xs) = Just x
          first (x, _, _) = x
          third (_, _, x) = x

optList = [ ('x', ( (\opts -> liftM (\res -> opts { hs_x = res } )
                            . readsMaybe )
            , "x position (c for center)", HSOption)
            )
          , ('y', ( (\opts -> liftM (\res -> opts { hs_y = res } )
                            . readsMaybe )
            , "y position (c for center)", HSOption)
            )
          , ('W', ( (\opts -> liftM (\res -> opts { hs_w = res } )
                            . readsMaybe )
            , "width (s for span)", HSOption)
            )
          , ('H', ( (\opts -> liftM (\res -> opts { hs_h = res } )
                            . readsMaybe )
            , "height (s for span)", HSOption)
            )
          , ('r', ( (\opts -> liftM (\res -> opts { hs_op = res } )
                            . readsMaybe )
            , "record as gif (in seconds)", HSOption)
            )
          , ('d', ( (\opts -> liftM (\res -> opts { hs_delay = res } )
                            . readsMaybe )
            , "delay between frames (in 1/100 of seconds)", HSOption)
            )
          , ('h', ( (\opts -> const $ Just $ opts { hs_help = True } )
            , "show help", HSSwitch)
            )
          ]

optMap = M.fromList optList

showHelp :: IO ()
showHelp = putStrLn "Here is help!"

invalidUsageMsg = "Invalid command line usage. Try -h for help."
