
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.List
import Data.Char
import System.IO
import qualified System.Directory as Dir
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import System.FilePath.Posix
import qualified Parcel
import qualified Cadastre
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified System.Random as Random
import Utils


type Pos = (Int, Int)

parcelPath :: String -> FilePath
parcelPath user = "./home/" ++ user ++ "/.cadastre/home.txt"


-- safely read a file: don't load too large files into memory
readUtf8File :: FilePath -> IO T.Text
readUtf8File path = do
    inputHandle <- openFile path ReadMode
    bytes <- BS.hGetNonBlocking inputHandle 8192
    return $ TE.decodeUtf8 bytes

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

-- System.Directory.listDirectory is to new to use
-- but this is the same code
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (Dir.getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

loadParcels :: IO Cadastre.Cadastre
loadParcels = do
    userNames <- listDirectory "home"
    let userPaths = map parcelPath userNames
    publicFiles <- listDirectory "public"
    let publicPaths = map ("./public/" ++) publicFiles
    let paths = userPaths ++ publicPaths ++ ["./adminparcel.prcl"] :: [FilePath]
    parcelTexts <- readFiles paths
    r <- Random.randomIO :: IO Int
    return 
        $ Cadastre.fromTexts r
            . map (\(path, text) -> (getOwnerFromPath path, text)) 
            $ parcelTexts

getOwnerFromPath :: FilePath -> Maybe T.Text
getOwnerFromPath path = case dropWhile (== ".") (splitDirectories path) of
    "home":owner:_xs -> Just $ T.pack owner
    "public":_xs -> Nothing
    "adminparcel.prcl":[] -> Just "@_admin"

makeJSONCadastre :: Cadastre.Cadastre -> BS.ByteString
makeJSONCadastre = BSL.toStrict . Aeson.encode


decodeJSONCadastre :: BS.ByteString -> Cadastre.Cadastre
decodeJSONCadastre jsonText = json
    where 
        Right json = decoded
        decoded = Aeson.eitherDecode (BSL.fromStrict jsonText) :: Either String Cadastre.Cadastre

readPreviousCadastre :: FilePath -> IO Cadastre.Cadastre
readPreviousCadastre path = do
    jsonText <- BS.readFile path
    let parcels = decodeJSONCadastre jsonText
    return parcels

writeSafe :: (FilePath -> a -> IO()) -> FilePath -> a -> IO()
writeSafe writer path dat = do
    let tempPath = path ++ ".tempfile"
    writer tempPath dat
    Dir.renameFile tempPath path

main :: IO ()
main = do
    p <- loadParcels
    r <- readPreviousCadastre "htown.json"
    
    let t = Cadastre.merge r p
    let json = makeJSONCadastre t
    let text = Cadastre.toText 25 25 t
    let html = Cadastre.toHtml 25 25 t
    
    writeSafe BS.writeFile "htown.json" json
    writeSafe TIO.writeFile "htown.txt" text
    writeSafe TIO.writeFile "htown.html" html
