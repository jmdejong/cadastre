
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parcel (
    Parcel(Parcel),
    empty,
    owner,
    location,
    fromText,
    hasOwner,
    toTextLines,
    charAtPos,
    linkAtPos,
    htmlAtPos,
    parcelWidth,
    parcelHeight,
    place
) where
--


import GHC.Generics
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Char
import Data.List
import Utils

type Text = T.Text

parcelWidth = 24
parcelHeight = 12

data Parcel = Parcel 
    { owner :: Maybe T.Text
    , location :: Pos -- todo: make this a Maybe ?
    , art :: [T.Text]
    , linkmask :: [T.Text]
    , links :: Map.Map Char Text
    } deriving (Generic, Show)
--

instance Aeson.ToJSON Parcel where 
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Parcel where 
    parseJSON = Aeson.withObject "Parcel" $ \v -> Parcel
        <$> (v Aeson..: "owner")
        <*> (pure (0, 0))
        <*> (v Aeson..: "art")
        <*> (v Aeson..: "linkmask")
        <*> (v Aeson..: "links")

fromText :: Maybe T.Text -> T.Text -> Parcel
fromText owner p = Parcel owner (x, y) plot linkMask links
    where
        (posl:l) = T.lines p
        [x, y] = map (read . T.unpack) (T.words posl) :: [Int]
        (plotLines, l') = splitAt parcelHeight l
        plot = (fillPlot parcelWidth parcelHeight plotLines) :: [T.Text]
        fillPlot :: Int -> Int -> [T.Text] -> [T.Text]
        fillPlot width height p = take height . map (fillLines width) $ p ++ repeat T.empty
        fillLines :: Int -> T.Text -> T.Text
        fillLines len line = T.take len $ T.append line $ T.replicate len " "
        (sepLine, rest) = splitAt 1 l'
        (linkMask, links) = parseSepLine sepLine
        parseSepLine :: [T.Text] -> ([T.Text], Map.Map Char T.Text)
        parseSepLine [] = (fillPlot parcelWidth parcelHeight [], Map.empty)
        parseSepLine [x]
            | (T.strip x) == "-" = (plot, makeLinkMap rest)
            | otherwise = (fillPlot parcelWidth parcelHeight maskLines, makeLinkMap linkLines)
                where (maskLines, linkLines) = splitAt parcelHeight rest
        makeLinkMap :: [T.Text] -> Map.Map Char T.Text
        makeLinkMap = Map.fromList . map parseLinkLine . filter (/= "")
        parseLinkLine :: T.Text -> (Char, T.Text)
        parseLinkLine line = (T.head begin, T.tail url)
            where (begin, url) = T.breakOn " " line

empty :: Parcel
empty = fromText Nothing "0 0"


toTextLines :: Parcel -> [Text]
toTextLines parcel = art parcel

hasOwner :: Parcel -> Bool
hasOwner parcel = case owner parcel of
                       Just _ -> True
                       Nothing -> False

charAtPos :: Parcel -> Pos -> Char
charAtPos parcel (x, y) = ((art parcel) !! y) `T.index` x

linkAtPos :: Parcel -> Pos -> Maybe Text
linkAtPos parcel (x, y) = Map.lookup linkChar (links parcel)
    where linkChar = ((linkmask parcel) !! y) `T.index` x


htmlAtPos :: Parcel -> Pos -> Text
htmlAtPos parcel pos@(x, y) = wrapId $ wrapLink $ htmlEscape $ T.pack [charAtPos parcel pos]
    where
        wrapId :: T.Text -> T.Text
        wrapId s = case (pos, owner parcel) of
            ((0, 0), Just o) -> "<span id=\"" `T.append` o `T.append` "\">" `T.append` s :: Text
            ((23, 0), Just _) -> s `T.append` "</span>" :: Text
            _ -> s
        wrapLink :: T.Text -> T.Text
        wrapLink s = case linkAtPos parcel pos of
            Just url -> "<a href=\"" `T.append` htmlEscape url `T.append` "\">" `T.append` s `T.append` "</a>" :: Text
            Nothing -> s :: Text

place :: Parcel -> Pos -> Parcel
place (Parcel owner _ plot linkMask links) pos = Parcel owner pos plot linkMask links


