

import Control.Exception
import Data.List
import Data.Char
-- import qualified Data.String.UTF8 as UTF8
import System.IO
import System.Directory
-- import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Text.JSON as JSON
-- import Text.JSON
import System.FilePath.Posix
import qualified Parcel
-- import qualified Data.Aeson as JSON


type Pos = (Int, Int)
-- type Parcel = (Maybe String, Pos, [String], [String], Map.Map Char String)

-- hasOwner :: Parcel -> Bool
-- hasOwner (Just owner, _, _, _, _) = True
-- hasOwner (Nothing, _, _, _, _) = False

hashPos :: Pos -> String
hashPos (x, y) = (show x) ++ "," ++ (show y)

parcelPath :: String -> FilePath
parcelPath user = "./home/" ++ user ++ "/.cadastre/home.txt"


readUtf8File :: FilePath -> IO String
readUtf8File path = do
    inputHandle <- openFile path ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle

appendFileIfReadable :: FilePath -> IO [(FilePath, String)] -> IO [(FilePath, String)]
appendFileIfReadable path iofiles =
    do
        files <- iofiles
        -- let path = getPath name :: FilePath
--         print path
        maybeFile <- try (readUtf8File path) :: IO (Either SomeException String)
        case maybeFile of
                Left _ex -> return files
                Right text -> do
                    return ((path, text):files)


readFiles :: [FilePath] -> IO [(FilePath, String)]
readFiles = foldr appendFileIfReadable (return [])


-- parseParcel :: Maybe String -> String -> Parcel
-- parseParcel owner p = (owner, (x, y), plot, linkMask, links)
--     where
--         (posl:l) = lines p
--         [x, y] = map read (words posl) :: [Int]
--         (plotLines, l') = splitAt 12 l
--         plot = fillPlot 24 12 plotLines
--         fillPlot width height p = take height . map (fillLines width) $ p ++ repeat ""
--         fillLines len line = take len $ line ++ repeat ' '
--         (sepLine, rest) = splitAt 1 l'
--         (linkMask, links) = parseSepLine sepLine
--         parseSepLine [] = (fillPlot 24 12 [], Map.empty)
--         parseSepLine [x]
--             | (strip x) == "-" = (plot, makeLinkMap rest)
--             | otherwise = (fillPlot 24 12 maskLines, makeLinkMap linkLines)
--                 where (maskLines, linkLines) = splitAt 12 rest
--         makeLinkMap = Map.fromList . map parseLinkLine . filter (/= "")
--         parseLinkLine line = (head begin, url)
--             where (begin, _sp:url) = break isSpace line
--         strip = dropWhile isSpace . dropWhileEnd isSpace

loadParcels :: IO [Parcel.Parcel]
loadParcels = do
    userNames <- listDirectory "home"
    let userPaths = map parcelPath userNames
    publicFiles <- listDirectory "public"
    let publicPaths = map ("./public/" ++) publicFiles
    let paths = userPaths ++ publicPaths ++ ["./adminparcel.prcl"]
    parcelTexts <- readFiles paths
    return $ map (\(path, text) -> Parcel.fromText (getOwnerFromPath path) text) parcelTexts

getOwnerFromPath :: FilePath -> Maybe String
getOwnerFromPath path = case dropWhile (==".") (splitDirectories path) of
    "home":owner:_xs -> Just owner
    "public":_xs -> Nothing
    "adminparcel.prcl":[] -> Just "@_admin"


parcelsToJSONObject :: [Parcel.Parcel] -> JSON.JSValue
parcelsToJSONObject = jnfo . map prepareParcel
    where
        prepareParcel parcel = (hashPos (Parcel.location parcel), Parcel.toJSON parcel)
--             (owner, pos@(x, y), art, linkmask, links) =
--             (hashpos pos,
--              jnfo [
--                 ("owner", case owner of
--                     Just name -> jnfs name
--                     Nothing -> JSON.JSNull),
--                 ("x", jnfn x),
--                 ("y", jnfn y),
--                 ("art", jnfa (map jnfs art)),
--                 ("linkmask", jnfa (map jnfs linkmask)),
--                 ("links", jnfo . map (\(k, v) -> ([k], jnfs v)) . Map.toList $ links)])
        jnfs = JSON.JSString . JSON.toJSString
        jnfn = JSON.JSRational False . toRational
        jnfo = JSON.JSObject . JSON.toJSObject
        jnfa = JSON.JSArray

makeJSONCadastre :: Real a => a -> [Parcel.Parcel] -> String
makeJSONCadastre seed parcels = JSON.encode . JSON.JSObject . JSON.toJSObject $ [
    ("places", parcelsToJSONObject parcels),
    ("seed", JSON.JSRational False . toRational $ seed),
    ("owners", JSON.JSObject . JSON.toJSObject . map makeOwnerTuple . filter Parcel.hasOwner $ parcels)]
    where
        makeOwnerTuple (Parcel.Parcel (Just owner) pos _ _ _) = (owner, JSON.JSString . JSON.toJSString . hashPos $ pos)


decodeJSONCadastre ::  String -> ([Parcel.Parcel], Rational)
decodeJSONCadastre jsonText = (parcels, seed)
    where
        JSON.Ok (JSON.JSObject jsObj) = JSON.decode jsonText :: JSON.Result JSON.JSValue
        properties = JSON.fromJSObject jsObj
        Just (JSON.JSRational False seed) = lookup "seed" properties
        Just (JSON.JSObject jsonParcels) = lookup "places" properties
        parcels = []

readPreviousCadastre :: String -> IO ([Parcel.Parcel], Rational)
readPreviousCadastre path = do
    jsonText <- readUtf8File path
    -- let Ok jsObj = JSON.decode jsonText
    let parcels = decodeJSONCadastre jsonText
    return parcels


main :: IO ()
main = do
    p <- loadParcels
    putStrLn $ makeJSONCadastre 0 p
--     return ()
