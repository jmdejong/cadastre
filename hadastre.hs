
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.List
import Data.Char
import System.IO
import System.Directory
import qualified Data.Map.Strict as Map
import qualified Text.JSON as JSON
import qualified Data.Aeson as Aeson
import System.FilePath.Posix
import qualified Parcel
import qualified Cadastre
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BS
import Utils


type Pos = (Int, Int)

parcelPath :: String -> FilePath
parcelPath user = "./home/" ++ user ++ "/.cadastre/home.txt"


readUtf8File :: FilePath -> IO T.Text
readUtf8File path = do
    inputHandle <- openFile path ReadMode
    hSetEncoding inputHandle utf8
    TIO.hGetContents inputHandle

appendFileIfReadable :: FilePath -> IO [(FilePath, T.Text)] -> IO [(FilePath, T.Text)]
appendFileIfReadable path iofiles =
    do
        files <- iofiles
        maybeFile <- try (readUtf8File path) :: IO (Either SomeException T.Text)
        case maybeFile of
                Left _ex -> return files
                Right text -> do
                    return ((path, text):files)


readFiles :: [FilePath] -> IO [(FilePath, T.Text)]
readFiles = foldr appendFileIfReadable (return [])


loadParcels :: IO Cadastre.Cadastre
loadParcels = do
    userNames <- listDirectory "home"
    let userPaths = map parcelPath userNames
    publicFiles <- listDirectory "public"
    let publicPaths = map ("./public/" ++) publicFiles
    let paths = userPaths ++ publicPaths ++ ["./adminparcel.prcl"] :: [FilePath]
    parcelTexts <- readFiles paths
    return 
        $ Cadastre.fromTexts 0
            . map (\(path, text) -> (getOwnerFromPath path, text)) 
            $ parcelTexts

getOwnerFromPath :: FilePath -> Maybe T.Text
getOwnerFromPath path = case dropWhile (== ".") (splitDirectories path) of
    "home":owner:_xs -> Just $ T.pack owner
    "public":_xs -> Nothing
    "adminparcel.prcl":[] -> Just "@_admin"

makeJSONCadastre :: Cadastre.Cadastre -> BS.ByteString
makeJSONCadastre = Aeson.encode


decodeJSONCadastre :: BS.ByteString -> Cadastre.Cadastre
decodeJSONCadastre jsonText = json
    where 
        Just json = Aeson.decode jsonText

readPreviousCadastre :: FilePath -> IO Cadastre.Cadastre
readPreviousCadastre path = do
    jsonText <- BS.readFile path
    let parcels = decodeJSONCadastre jsonText
    return parcels


main :: IO ()
main = do
    p <- loadParcels
--     putStrLn $ makeJSONCadastre p
--     putStrLn $ Cadastre.toText 5 5 p
    let json = makeJSONCadastre p
        text = Cadastre.toText 25 25 p
        html = Cadastre.toHtml 25 25 p
    
--     print $ length json
--     print $ length text
--     print $ BC.length html
    
    BS.writeFile "htown.json" json
    TIO.writeFile "htown.txt" text
    TIO.writeFile "htown.html" html
