

import Control.Exception
import Data.List
import Data.Char
import System.IO
import System.Directory
import qualified Data.Map.Strict as Map
import qualified Text.JSON as JSON
import System.FilePath.Posix
import qualified Parcel
import qualified Cadastre


type Pos = (Int, Int)

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
        maybeFile <- try (readUtf8File path) :: IO (Either SomeException String)
        case maybeFile of
                Left _ex -> return files
                Right text -> do
                    return ((path, text):files)


readFiles :: [FilePath] -> IO [(FilePath, String)]
readFiles = foldr appendFileIfReadable (return [])


loadParcels :: IO Cadastre.Cadastre
loadParcels = do
    userNames <- listDirectory "home"
    let userPaths = map parcelPath userNames
    publicFiles <- listDirectory "public"
    let publicPaths = map ("./public/" ++) publicFiles
    let paths = userPaths ++ publicPaths ++ ["./adminparcel.prcl"]
    parcelTexts <- readFiles paths
    return 
        $ Cadastre.fromTexts 0
            . map (\(path, text) -> (getOwnerFromPath path, text)) 
            $ parcelTexts

getOwnerFromPath :: FilePath -> Maybe String
getOwnerFromPath path = case dropWhile (==".") (splitDirectories path) of
    "home":owner:_xs -> Just owner
    "public":_xs -> Nothing
    "adminparcel.prcl":[] -> Just "@_admin"

makeJSONCadastre :: Cadastre.Cadastre -> String
makeJSONCadastre = JSON.encode . Cadastre.toJSON


decodeJSONCadastre ::  String -> Cadastre.Cadastre
decodeJSONCadastre jsonText = Cadastre.fromJSON json
    where 
        JSON.Ok json = JSON.decode jsonText

readPreviousCadastre :: String -> IO Cadastre.Cadastre
readPreviousCadastre path = do
    jsonText <- readUtf8File path
    let parcels = decodeJSONCadastre jsonText
    return parcels


main :: IO ()
main = do
    p <- loadParcels
    putStrLn $ makeJSONCadastre p
    
