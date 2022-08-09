module HSOptions ( getDim
                 , getPos
                 , parseArgs
                 , runHSOperation
                 , showHelp
                 , defOptions
                 , invalidUsageMsg
                 , HShotOptions(..)
                 , readConfig
                 , getExtension
                 ) where

import Graphics.X11 (Position, Dimension)

import Data.Bool (bool)
import qualified Data.Map as M ( Map, fromList, (!?), insert
                               , foldrWithKey, empty)
import Data.List (lines, unlines)

import Control.Monad (liftM, mfilter, (<=<))

import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing
                        , doesFileExist)
import System.FilePath ((</>), (<.>))

appName         = "haskshot"
defConfigName   = appName <.> "conf"
defRecTime      = 10 :: Int
confSep         = '='

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
          , ('t', ( (\opts -> const $ Just $ opts { hs_test = True } )
            , "test / debug mode", HSSwitch)
            )
          , ('h', ( (\opts -> const $ Just $ opts { hs_help = True } )
            , "show help", HSSwitch)
            )
          ]

optMap = M.fromList optList

argPos pos = case pos of
    HSCoord c   -> show c
    HSCenter    -> "c"

argDim dim = case dim of
    HSSize s    -> show s
    HSSpan      -> "s"

getConfigParameters = [ ("rec_time",    (show . recTime,    'r'))
                      , ("delay",       (show . hs_delay,   'd'))
                      , ("x",           (argPos . hs_x,     'x'))
                      , ("y",           (argPos . hs_y,     'y'))
                      , ("width",       (argDim . hs_w,     'W'))
                      , ("height",      (argDim . hs_h,     'H'))
                      ]

data HShotOptions = HShotOptions { hs_x     :: HSPosition
                                 , hs_y     :: HSPosition
                                 , hs_w     :: HSDimension
                                 , hs_h     :: HSDimension
                                 , hs_op    :: HSOperation
                                 , hs_delay :: Int
                                 , hs_path  :: FilePath
                                 , hs_help  :: Bool
                                 , hs_test  :: Bool
                                 } deriving Show

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

getExtension :: HShotOptions -> String
getExtension opts = case hs_op opts of
    HSScreenshot    -> "bmp"
    HSRecord _      -> "gif"

defOptions :: HShotOptions
defOptions = HShotOptions (HSCoord 0) (HSCoord 0) HSSpan HSSpan
                          HSScreenshot 50 "" False False

splitConf :: String -> [String]
splitConf str = case dropWhile (== confSep) str of
                    "" -> []
                    s' -> w : splitConf s''
                          where (w, s'') = break (==confSep) s'

parseConfig' :: [String] -> M.Map Char String -> Maybe HShotOptions
parseConfig' [] part = M.foldrWithKey (\k x acc -> (\(fn, _, _) -> (flip fn) x
                                                               =<< acc)
                                               =<< (M.!?) optMap k)
                                      (Just defOptions) part

parseConfig' (x:xs) part = case splitConf x of

    [key, val]  -> parseConfig' xs =<< (\key' -> Just $ M.insert key' val part)
                                   =<< ((M.!?) confMap key)
    _           -> Nothing

    where confMap = M.fromList
                  . map (\(k1, (_, k2)) -> (k1, k2))
                  $ getConfigParameters

recTime opts = case hs_op opts of
    HSScreenshot    -> defRecTime
    HSRecord time   -> time

writeConfigFromOptions :: FilePath -> HShotOptions -> IO ()
writeConfigFromOptions fp opts = writeFile fp
                               . unlines
                               . map (\(k, (f, _)) -> k ++ (confSep:f opts))
                               $ getConfigParameters

parseConfig :: String -> HShotOptions
parseConfig = maybe defOptions id . (flip parseConfig') M.empty . lines

readConfig :: IO HShotOptions
readConfig = do
    dir <- getAppUserDataDirectory appName
    createDirectoryIfMissing False dir
    (\fp -> (\opts -> return $ opts { hs_path = dir })
        =<< bool (const (return defOptions)
                    =<< writeConfigFromOptions fp defOptions)
                 (liftM parseConfig $ readFile fp)
        =<< doesFileExist fp) . (</>) dir
                              $ defConfigName

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

parseArgs' :: [String] -> HShotOptions -> Maybe HShotOptions
parseArgs' [] part = Just part
parseArgs' (x:xs) part = (mfilter (=='-') . head' . take 1) x
                     >>= const ((!?) x 1)
                     >>= (M.!?) optMap
                     >>= \tpl -> parseArgs' (runOptMode (tail' xs) xs
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

parseArgs :: [String] -> HShotOptions -> Maybe HShotOptions
parseArgs args opts = parseArgs' args opts
                  >>= \opts' -> Just $ bool (opts' { hs_op = HSScreenshot })
                                            opts'
                                     $ elem "-r" args

showHelpFor ch = map (\(opt, (_, help, _)) -> "\t" ++ (ch:opt:"\t") ++ help)

showHelp :: HShotOptions -> IO ()
showHelp opts = mapM_ putStrLn $ concat
    [ [ "haskshot - minimalistic screenshot and gif recording application"
      , "           for X server"
      , ""
      , "haskshot [-r <time>] [-d <gif delay>]"
      , "\t[-x <x position>] [-y <y position>]"
      , "\t[-W <width>] [-H <height>] [-t] [-h]"
      , ""
      , "Options:"
      ] , showHelpFor '-' optList,
      [ ""
      , "Example 1 (take screenshot):"
      , "\thaskshot"
      , ""
      , "Example 2 (record gif for 15s):"
      , "\thaskshot -r 15"
      , ""
      , "Configuration file:"
      , "\t" ++ show ((hs_path opts) </> defConfigName)
      , ""
      , "Note: If the configuration file does not exist, it will be created"
      , "on the first run with default parameter values."
      ]
    ]

invalidUsageMsg = "Invalid command line usage. Try -h for help."
