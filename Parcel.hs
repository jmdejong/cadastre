module Parcel (
    Parcel(Parcel),
    empty,
    owner,
    location,
    fromText,
    toJSON,
    fromJSON,
    hasOwner,
    toTextLines,
    charAtPos,
    linkAtPos,
    parcelWidth,
    parcelHeight
) where
--

import qualified Text.JSON as JSON
import qualified Data.Map as Map
import Data.Char
import Data.List
import Utils


parcelWidth = 24
parcelHeight = 12

data Parcel = Parcel 
    { owner :: Maybe String
    , location :: Pos 
    , plot :: [String]
    , linkMask :: [String]
    , links :: Map.Map Char String
    } deriving Show
--



fromText :: Maybe String -> String -> Parcel
fromText owner p = Parcel owner (x, y) plot linkMask links
    where
        (posl:l) = lines p
        [x, y] = map read (words posl) :: [Int]
        (plotLines, l') = splitAt parcelHeight l
        plot = fillPlot parcelWidth parcelHeight plotLines
        fillPlot width height p = take height . map (fillLines width) $ p ++ repeat ""
        fillLines len line = take len $ line ++ repeat ' '
        (sepLine, rest) = splitAt 1 l'
        (linkMask, links) = parseSepLine sepLine
        parseSepLine [] = (fillPlot parcelWidth parcelHeight [], Map.empty)
        parseSepLine [x]
            | (strip x) == "-" = (plot, makeLinkMap rest)
            | otherwise = (fillPlot parcelWidth parcelHeight maskLines, makeLinkMap linkLines)
                where (maskLines, linkLines) = splitAt parcelHeight rest
        makeLinkMap = Map.fromList . map parseLinkLine . filter (/= "")
        parseLinkLine line = (head begin, url)
            where (begin, _sp:url) = break isSpace line
        strip = dropWhile isSpace . dropWhileEnd isSpace

empty :: Parcel
empty = fromText Nothing "0 0"

toJSON :: Parcel -> JSON.JSValue
toJSON (Parcel owner pos@(x, y) art linkmask links) = jfo [
    ("owner", case owner of
        Just name -> jfs name
        Nothing -> JSON.JSNull),
    ("x", jfn x),
    ("y", jfn y),
    ("art", jfa (map jfs art)),
    ("linkmask", jfa (map jfs linkmask)),
    ("links", jfo . map (\(k, v) -> ([k], jfs v)) . Map.toList $ links)]
    where 
        jfs = JSON.JSString . JSON.toJSString
        jfn = JSON.JSRational False . toRational
        jfo = JSON.JSObject . JSON.toJSObject
        jfa = JSON.JSArray
--


fromJSON :: JSON.JSValue -> Parcel
fromJSON j = Parcel owner (x, y) art linkmap links
    where
        owner = case ownerObj of
            JSON.JSString s -> Just (JSON.fromJSString s)
            JSON.JSNull -> Nothing
        ownerObj = assume $ lookup "owner" o
        x = floor (jtn xval) :: Int
        y = floor (jtn yval) :: Int
        xval = assume $ lookup "x" o
        yval = assume $ lookup "y" o
        art = map jts . jta . assume . lookup "art" $ o
        linkmap = map jts . jta . assume . lookup "linkmask" $ o
        links = Map.fromList . map (\(s, v) -> (head s, jts v)) $ linkKeyList
        linkKeyList = (jto . assume . lookup "links" $ o) :: [(String, JSON.JSValue)]
        o = jto j
        jts (JSON.JSString j) = JSON.fromJSString j
        jtn (JSON.JSRational False j) = j
        jto (JSON.JSObject j) = JSON.fromJSObject j
        jta (JSON.JSArray j) = j


toTextLines :: Parcel -> [String]
toTextLines parcel = plot parcel

hasOwner :: Parcel -> Bool
hasOwner parcel = case owner parcel of
                       Just _ -> True
                       Nothing -> False

charAtPos :: Parcel -> Pos -> Char
charAtPos parcel (x, y) = ((plot parcel) !! y) !! x

linkAtPos :: Parcel -> Pos -> Maybe String
linkAtPos parcel (x, y) = Map.lookup linkChar (links parcel)
    where linkChar = ((linkMask parcel) !! y) !! x






