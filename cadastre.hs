

import Control.Exception
import Data.List
import Data.Char
import qualified Data.String.UTF8 as UTF8
import System.IO
import System.Directory
-- import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
-- import qualified Text.JSON as JSON
import Text.JSON
-- import qualified Data.Aeson as JSON


type Pos = (Int, Int)
type Parcel = (String, Pos, [String], [String], Map.Map Char String)

hashpos :: Pos -> String
hashpos (x, y) = (show x) ++ "," ++ (show y)

parcelPath :: String -> FilePath
parcelPath user = "./home/" ++ user ++ "/.cadastre/home.txt"


-- getParcelFileNames :: IO [String]
-- getParcelFileNames = do
--     return []
-- 
-- generalizeIO :: [IO (a, b)] -> IO [(a, b)]
-- generalizeIO [] = do return []
-- generalizeIO (x:xs) = do
--     (y, z) <-x
--     ys <- generalizeIO xs
--     return ((y, z):ys)
-- 
-- filterEx :: [(Either SomeException a, b)] -> [(a, b)]
-- filterEx [] = []
-- filterEx ((x,y):xs) = 
--     case x of 
--         Left _ex -> filterEx xs
--         Right v -> (v, y):(filterEx xs)


-- readParcels :: IO [Parcel]
-- readParcels = do
--     filesNames <- getParcelFileNames
--     let texts = filterMaybe . map (try.readFile) $ fileNames
--     return . map readParcel . filter hasParcel $ files
-- 

readUtf8File :: FilePath -> IO String
readUtf8File path = do
    inputHandle <- openFile path ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle

appendFileIfReadable :: (a -> FilePath) -> a -> IO [(a, String)] -> IO [(a, String)]
appendFileIfReadable getPath name iofiles = 
    do
        files <- iofiles
        let path = getPath name :: FilePath
--         print path
        maybeFile <- try (readUtf8File path) :: IO (Either SomeException String)
        case maybeFile of
                Left _ex -> return files
                Right text -> do
                    return ((name, text):files)
                 

readFiles :: (a -> FilePath) -> [a] -> IO [(a, String)]
readFiles getPath = foldr (appendFileIfReadable getPath) (return [])


parseParcel :: String -> String -> Parcel
parseParcel owner p = (owner, (x, y), plot, linkMask, links)
    where
        (posl:l) = lines p
        [x, y] = map read (words posl) :: [Int]
        (plotLines, l') = splitAt 12 l
        plot = fillPlot 24 12 plotLines
        fillPlot width height p = take height . map (fillLines width) $ p ++ repeat ""
        fillLines len line = take len $ line ++ repeat ' '
        (sepLine, rest) = splitAt 1 l'
        (linkMask, links) = parseSepLine sepLine
        parseSepLine [] = (fillPlot 24 12 [], Map.empty)
        parseSepLine [x] 
            | (strip x) == "-" = (plot, makeLinkMap rest)
            | otherwise = (fillPlot 24 12 maskLines, makeLinkMap linkLines)
                where (maskLines, linkLines) = splitAt 12 rest
        makeLinkMap = Map.fromList . map parseLinkLine . filter (/= "")
        parseLinkLine line = (head begin, url)
            where (begin, _sp:url) = break isSpace line
        strip = dropWhile isSpace . dropWhileEnd isSpace

loadParcels :: IO [Parcel]
loadParcels = do 
    userNames <- listDirectory "home"
    parcelTexts <- readFiles parcelPath userNames
    return $ map (\(name, text) -> parseParcel name text) parcelTexts


parcelsToJSONObject :: [Parcel] -> JSValue
parcelsToJSONObject = jnfo . map prepareParcel
    where 
        prepareParcel (owner, pos@(x, y), art, linkmask, links) = 
            (hashpos pos, 
             jnfo [
                ("owner", jnfs owner),
                ("x", jnfn x),
                ("y", jnfn y),
                ("art", jnfa (map jnfs art)),
                ("linkmask", jnfa (map jnfs linkmask)),
                ("links", jnfo . map (\(k, v) -> ([k], jnfs v)) . Map.toList $ links)])
        jnfs = JSString . toJSString
        jnfn = JSRational False . toRational
        jnfo = JSObject . toJSObject
        jnfa = JSArray

makeJSONCadastre :: Real a => a -> [Parcel] -> String
makeJSONCadastre seed parcels = encode . JSObject . toJSObject $ [
    ("parcels", parcelsToJSONObject parcels),
    ("seed", JSRational False . toRational $ seed),
    ("owners", JSObject . toJSObject . map makeOwnerTuple $ parcels)]
    where
        makeOwnerTuple (owner, pos, _, _, _) = (owner, JSString . toJSString . hashpos $ pos)
    


-- main :: IO ()
-- main = do 
-- --     print $ case decode "[1,2,3]" of 
-- --                  Ok s -> s
-- --                  Error s -> "ERROR: " ++ s
-- --     userNames <- listDirectory "home"
-- --     
-- --     m <- readFiles (\t -> "./home/" ++ t ++ "/.cadastre/home.txt") userNames
--     p <- loadParcels
-- --     let (user, plot) = m !! 4
--     print $ makeJSONCadastre 0 p
-- --     putStrLn . show . map (\(name, text) -> name) $ m
-- --     inputs <- readParcels
-- --     parcels <- map parseParcel inputs
-- -- --     old
--     return ()
    
