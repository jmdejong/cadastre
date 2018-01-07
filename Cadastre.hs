
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Cadastre (
    Cadastre,
    empty,
    fromTexts,
    toText,
    toHtml,
    merge
) where

import GHC.Generics
import qualified Data.Text as T
import qualified Parcel
import Data.List
import Utils
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import Data.Bits
import qualified Data.Binary as Bin
import qualified Data.Array as Arr
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Semigroup (Semigroup ((<>)))
import Data.Scientific

data Cadastre = Cadastre 
    { parcels :: Map.Map Pos Parcel.Parcel
    , bg :: Background}
    deriving (Generic, Show)


data Background = Background
    { bgseed :: Int
    , grid :: Arr.Array Bin.Word16 Bin.Word16}
    deriving Show

type FilledCadastre = (Cadastre, Background)

instance Aeson.ToJSON Cadastre where
    toJSON (Cadastre parcels background) =
        Aeson.object ["places" Aeson..= (Map.mapKeys hashPos parcels), "seed" Aeson..= background]

    -- this encodes directly to a bytestring Builder
    toEncoding (Cadastre parcels background) =
        Aeson.pairs ("places" Aeson..= (Map.mapKeys hashPos parcels) <> "seed" Aeson..= background)
    

instance Aeson.FromJSON Cadastre where
    parseJSON = Aeson.withObject "Cadastre" $ \v -> Cadastre
        <$> (fmap (mapKeyValue (\pos parcel -> (unHashPos pos, Parcel.place parcel $ unHashPos pos))) (v Aeson..: "places"))
        <*> (v Aeson..: "seed")

instance Aeson.ToJSON Background where
    toJSON (Background seed _) = Aeson.toJSON seed
    toEncoding (Background seed _) = Aeson.toEncoding seed

instance Aeson.FromJSON Background where
    parseJSON = Aeson.withScientific "Background" $  return . makeBackground . def 0 . (toBoundedInteger :: Scientific -> Maybe Int)

empty :: Cadastre
empty = Cadastre Map.empty (makeBackground 0)

seed :: Cadastre -> Int
seed (Cadastre _ (Background s _)) = s

fromTexts :: Int -> [(Maybe T.Text, T.Text)] -> Cadastre
fromTexts seed texts = Cadastre (Map.fromList $ filterMaybe $ map parseText texts) (makeBackground seed)
    where
        parseText :: (Maybe T.Text, T.Text) -> Maybe (Pos, Parcel.Parcel)
        parseText (owner, text) = case Parcel.fromText owner text of
                                       Just parcel -> Just (Parcel.location parcel, parcel)
                                       Nothing -> Nothing


merge :: Cadastre -> Cadastre -> Cadastre
merge (Cadastre oldP _) (Cadastre newP seed) = Cadastre mergedP seed
    where 
        mergedP = Map.filter isAllowed newP
        isAllowed parcel = case Map.lookup (Parcel.location parcel) oldP of 
            Just p -> 
                case (Parcel.owner parcel, Parcel.owner p) of
                     (Just o1, Just o2) -> o1 == o2
                     (Nothing, Just o2) -> False
                     (_, Nothing) -> True
            Nothing -> True


toText :: Int -> Int -> Cadastre -> T.Text
toText width height = T.unlines . map T.pack . outputRegion charAtPos width height

charAtPos :: Cadastre -> Pos -> Char
charAtPos cadastre pos = fst $ charLinkAtPos cadastre pos

charLinkAtPos  :: Cadastre -> Pos -> (Char, Maybe T.Text)
charLinkAtPos cadastre (x, y) = case Map.lookup (div x Parcel.parcelWidth, div y Parcel.parcelHeight) (parcels cadastre)  of
    Just parcel -> (Parcel.charAtPos parcel localPos, Parcel.linkAtPos parcel localPos)
        where localPos = (mod x Parcel.parcelWidth, mod y Parcel.parcelHeight)
    Nothing -> (getBackground x y cadastre, Nothing)


outputRegion :: (Cadastre -> Pos -> a) -> Int -> Int -> Cadastre -> [[a]]
outputRegion posFun width height cadastre = 
    take (height * Parcel.parcelHeight) (
        map 
            (\y -> take (width * Parcel.parcelWidth) (
                map 
                    (\x -> posFun cadastre (x, y)) 
                    [0..]))
            [0..])
--

htmlBegin = "<!DOCTYPE html>\n<html>\n<!-- See tilde.town/~troido/cadastre for instructions -->\n<head>\n    <meta charset='utf-8'>\n   \n<style>\na {text-decoration: none}\n    </style>\n</head>\n<body>\n    <pre>" :: T.Text
htmlEnd = "</body>\n<!-- Cadastre made by ~Troido; art by tilde.town users -->\n</html>\n" :: T.Text

newline = "\n" :: T.Text

toHtml :: Int -> Int -> Cadastre -> T.Text -- BU.ByteString
toHtml width height = makePage . T.intercalate newline . map T.concat . outputRegion htmlAtPos width height
    where makePage s = T.concat [htmlBegin, s, htmlEnd]

htmlAtPos :: Cadastre -> Pos -> T.Text -- BU.ByteString --String
htmlAtPos cadastre (x, y) = if localPos == (0,0) then T.append idSpan posString else posString
    where
        posString = case p of 
            Just parcel -> Parcel.htmlAtPos parcel localPos
            Nothing -> htmlEscape $ T.singleton $ getBackground x y cadastre
        localPos = (mod x Parcel.parcelWidth, mod y Parcel.parcelHeight)
        parcelPos@(parcelX, parcelY) = (div x Parcel.parcelWidth, div y Parcel.parcelHeight)
        p = Map.lookup parcelPos (parcels cadastre)
        idSpan = "<span id=\"" `T.append` T.pack (show parcelX) `T.append` "," `T.append` T.pack (show parcelY) `T.append` "\"></span>"

getBackground :: Int -> Int -> Cadastre -> Char
getBackground x y (Cadastre _ (Background _ grid)) = C.index backgroundChars $ (`mod` 256) $ fromIntegral $ (Arr.!) grid $ fromIntegral $ (mod x 256) + 256 * (mod y 256)
        
backgroundChars :: C.ByteString
backgroundChars = C.pack $ take 256 $ "' ' ' ' , , , , . . . . ` ` ` ` \" \"" ++ repeat ' '


galoisStep :: (Integral i, Bits i )=> i -> i
galoisStep i = if testBit i 0 then (shiftR i 1) `xor` 0xB400 else shiftR i 1

galois :: Bin.Word16 -> Arr.Array Bin.Word16 Bin.Word16
galois start = Arr.listArray (0, 0xFFFF) $ iterate galoisStep start


makeBackground :: Int -> Background
makeBackground 0 = makeBackground 1
makeBackground seed = Background seed (galois $ fromIntegral seed)


hashPos :: Pos -> T.Text
hashPos (x, y) = T.pack $ (show x) ++ "," ++ (show y)

unHashPos :: T.Text -> Pos
unHashPos t = (read (T.unpack begin), read (T.unpack end))
    where
        (begin, rest) = T.breakOn "," t
        end = T.tail rest

mapKeyValue :: Ord k2 => (k1 -> a -> (k2, b)) -> Map.Map k1 a -> Map.Map k2 b
mapKeyValue fn = Map.foldrWithKey update Map.empty
    where
        update key value m = Map.insert newKey newValue m
            where
                (newKey, newValue) = fn key value


