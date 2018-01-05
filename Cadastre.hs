
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Cadastre (
    Cadastre,
    empty,
    fromTexts,
    toText,
    toHtml
) where

import GHC.Generics
import qualified Data.Text as T
import qualified Parcel
import Data.List
import Utils
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

data Cadastre = Cadastre 
    { parcels :: Map.Map Pos Parcel.Parcel
    , seed :: Integer}
    deriving (Generic, Show)

instance Aeson.ToJSON Cadastre where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Cadastre


empty :: Cadastre
empty = Cadastre Map.empty 0

fromTexts :: Integer -> [(Maybe T.Text, T.Text)] -> Cadastre
fromTexts seed texts = Cadastre (Map.fromList $ map parseText texts) seed
    where
        parseText (owner, text) = (Parcel.location parcel, parcel)
            where 
                parcel = Parcel.fromText owner text


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


toText :: Int -> Int -> Cadastre -> T.Text
toText width height = T.unlines . map T.pack . outputRegion charAtPos width height

charAtPos :: Cadastre -> Pos -> Char
charAtPos cadastre pos = fst $ charLinkAtPos cadastre pos

charLinkAtPos  :: Cadastre -> Pos -> (Char, Maybe T.Text)
charLinkAtPos cadastre (x, y) = case Map.lookup (div x Parcel.parcelWidth, div y Parcel.parcelHeight) (parcels cadastre)  of
    Just parcel -> (Parcel.charAtPos parcel localPos, Parcel.linkAtPos parcel localPos)
        where localPos = (mod x Parcel.parcelWidth, mod y Parcel.parcelHeight)
    Nothing -> (randomChar x y (seed cadastre), Nothing)


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
            Nothing -> htmlEscape $ T.singleton $ randomChar x y (seed cadastre)
        localPos = (mod x Parcel.parcelWidth, mod y Parcel.parcelHeight)
        parcelPos@(parcelX, parcelY) = (div x Parcel.parcelWidth, div y Parcel.parcelHeight)
        p = Map.lookup parcelPos (parcels cadastre)
        idSpan = "<span id=\"" `T.append` T.pack (show parcelX) `T.append` "," `T.append` T.pack (show parcelY) `T.append` "\"></span>"


backgroundChars :: [Char]
backgroundChars = take 256 $ "'''',,,,....````\"\"" ++ repeat ' '

randomChar :: Int -> Int -> Integer -> Char
randomChar x y s = backgroundChars !! random
    where random = fromIntegral . randomize . intersperse ' ' . concatMap show $ [x, y, fromIntegral s]





hashPos :: Pos -> String
hashPos (x, y) = (show x) ++ "," ++ (show y)



