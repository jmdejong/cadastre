
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
import Data.Bits
import qualified Data.Binary as Bin
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteArray as BA
import Data.Scientific

data Cadastre = Cadastre 
    { parcels :: Map.Map Pos Parcel.Parcel
    , bg :: Background}
    deriving (Generic, Show)


data Background = Background
    { bgseed :: Integer
    , grid :: Array Bin.Word16 Bin.Word16}
    deriving Show

type FilledCadastre = (Cadastre, Background)

instance Aeson.ToJSON Cadastre where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Cadastre

instance Aeson.ToJSON Background where
    toJSON (Background seed _) = Aeson.toJSON seed
    toEncoding (Background seed _) = Aeson.toEncoding seed

instance Aeson.FromJSON Background where
    parseJSON = Aeson.withScientific "Background" $  return . makeBackground . fromIntegral . (assume . toBoundedInteger :: Scientific -> Int)

empty :: Cadastre
empty = Cadastre Map.empty (makeBackground 0)

seed :: Cadastre -> Integer
seed (Cadastre _ (Background s _)) = s

fromTexts :: Integer -> [(Maybe T.Text, T.Text)] -> Cadastre
fromTexts seed texts = Cadastre (Map.fromList $ map parseText texts) (makeBackground seed)
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
getBackground x y (Cadastre _ (Background _ grid)) = C.index backgroundChars $ (`mod` 256) $ fromIntegral $ (!) grid $ fromIntegral $ (mod x 256) + 256 * (mod y 256)
        
backgroundChars :: C.ByteString
backgroundChars = C.pack $ take 256 $ "' ' ' ' , , , , . . . . ` ` ` ` \" \"" ++ repeat ' '


galoisStep :: (Integral i, Bits i )=> i -> i
galoisStep i = if testBit i 0 then (shiftR i 1) `xor` 0xB400 else shiftR i 1

galois :: Bin.Word16 -> Array Bin.Word16 Bin.Word16
galois start = listArray (0, 0xFFFF) $ iterate galoisStep start
--     where
--         step i = case i .&. 1 of 
--                       1 -> (shiftR i 1) `xor` 0xB400
--                       _ -> shiftR i 1

-- background :: Array Bin.Word16 Bin.Word16
-- background = galois 1
-- background = background' 11
--     where
--         background' start = listArray (0, 65535) $ iterate step start
--         step i = case i .&. 1 of 
--                       1 -> (shiftR i 1) `xor` 0xB400
--                       _ -> shiftR i 1
--array (0, 2^16) $ map (\i -> (i, fizzle $ fromIntegral i)) [0..(2^16-1)]
--     where 
--         fizzle :: Bin.Word16 -> Bin.Word16
--         fizzle 0xACE1 = 0
--         fizzle i = debugPrint i $ 1 + background ! (fromIntegral (step i))
--         step :: Bin.Word16 -> Bin.Word16
--         step i = case i .&. 1 of 
--                       1 -> (shiftR i 1) `xor` 0xB400
--                       _ -> shiftR i 1

-- randomChar :: Int -> Int -> Integer -> Char
-- randomChar x y s = C.index backgroundChars $ fromIntegral (mod random 256)
--     where -- an improvised hashing function. I'm looking for any better function
--         x8 = fromIntegral x :: Bin.Word16
--         y8 = fromIntegral y :: Bin.Word16
--         s8 = fromIntegral s :: Bin.Word16
--         random = mod ((galois 1) ! ((x8 `mod` 256) + 256 * (y8 `mod` 256))) 256--(mod r 256) `xor` (div r 256) --(galois (11+(galois (s8 + 1) ! y8))) ! x8  
--         r = galoisStep (galoisStep x8) `xor` galoisStep y8-- (x8+11) * (step (y8+10)) `xor` s8


makeBackground :: Integer -> Background
makeBackground 0 = makeBackground 1
makeBackground seed = Background seed (galois $ fromIntegral seed)


hashPos :: Pos -> String
hashPos (x, y) = (show x) ++ "," ++ (show y)



