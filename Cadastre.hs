module Cadastre (
    Cadastre,
    empty,
    fromTexts,
    toJSON,
    fromJSON,
    toText
) where
--
import qualified Parcel
import qualified Text.JSON as JSON
import Data.List
import Utils
import qualified Data.Map as Map
import Crypto.Hash
import Data.ByteString.Char8 (pack)
import qualified Data.ByteArray as BA
import qualified Data.Binary as Bin

data Cadastre = Cadastre 
    { parcels :: Map.Map Pos Parcel.Parcel
    , seed :: Integer}
    deriving Show


empty :: Cadastre
empty = Cadastre Map.empty 0

fromTexts :: Integer -> [(Maybe String, String)] -> Cadastre
fromTexts seed texts = Cadastre (Map.fromList $ map parseText texts) seed
    where
        parseText (owner, text) = (Parcel.location parcel, parcel)
            where 
                parcel = Parcel.fromText owner text

toJSON :: Cadastre -> JSON.JSValue
toJSON (Cadastre parcelMap seed) = jnfo $ [
    ("places", parcelsToJSONObject parcels),
    ("seed", jnfn $ seed),
    ("owners", jnfo . map makeOwnerTuple . filter Parcel.hasOwner $ parcels)]
    where
        makeOwnerTuple (Parcel.Parcel (Just owner) pos _ _ _) = (owner, jnfs . hashPos $ pos)
        parcelsToJSONObject = jnfo . map prepareParcel
        prepareParcel parcel = (hashPos (Parcel.location parcel), Parcel.toJSON parcel)
        jnfs = JSON.JSString . JSON.toJSString
        jnfn = JSON.JSRational False . toRational
        jnfo = JSON.JSObject . JSON.toJSObject
        jnfa = JSON.JSArray
        parcels = Map.elems parcelMap

fromJSON :: JSON.JSValue -> Cadastre
fromJSON (JSON.JSObject jso) = Cadastre parcelmap (floor seedVal)
    where 
        JSON.JSRational false seedVal = assume . lookup "seed" $ o
        parcelmap = Map.fromList $ map (\p -> (Parcel.location p, p)) parcels
        parcels = map Parcel.fromJSON $ jsParcels
        jsParcels = map snd . JSON.fromJSObject $ parcelJso
        Just (JSON.JSObject parcelJso) = lookup "places" o
        o = JSON.fromJSObject jso


merge :: Cadastre -> Cadastre -> Cadastre
merge (Cadastre oldP _) (Cadastre newP seed) = Cadastre mergedP seed
    where 
        mergedP = Map.filter isAllowed newP
        isAllowed parcel = case oldP Map.!? (Parcel.location parcel) of 
            Just p -> 
                case (Parcel.owner parcel, Parcel.owner p) of
                     (Just o1, Just o2) -> o1 == o2
                     (Nothing, Just o2) -> False
                     (_, Nothing) -> True
            Nothing -> True


toText :: Int -> Int -> Cadastre -> String
toText width height cadastre = unlines $ map (\y -> map (\x -> charAtPos cadastre x y) [0..(width-1)]) [0..(height-1)]

charAtPos :: Cadastre -> Int -> Int -> Char
charAtPos cadastre x y = case Map.lookup (div x Parcel.parcelWidth, div y Parcel.parcelHeight) (parcels cadastre)  of
    Just parcel -> Parcel.charAtGlobalPos parcel (x, y)
    Nothing -> randomChar x y (seed cadastre)


backgroundChars :: [Char]
backgroundChars = take 256 $ "'''',,,,....````\"\"" ++ repeat ' '

randomChar :: Int -> Int -> Integer -> Char
randomChar x y s = backgroundChars !! random
    where random = fromIntegral . randomize . intersperse ' ' . concatMap show $ [x, y, fromIntegral s]


randomize :: String -> Bin.Word8
randomize = (`BA.index` 0) .  md5 . pack . intersperse ' ' . concatMap show
    where 
        md5 x = hash x :: Digest MD5




hashPos :: Pos -> String
hashPos (x, y) = (show x) ++ "," ++ (show y)



