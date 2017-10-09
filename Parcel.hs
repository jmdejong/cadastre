module Parcel (
    Parcel(Parcel),
    owner,
    location,
    fromText,
    toJSON,
    fromJSON,
    hasOwner
) where
--

import qualified Text.JSON as JSON
import qualified Data.Map as Map
import Data.Char
import Data.List



data Parcel = Parcel 
    { owner :: Maybe String
    , location :: (Int, Int) 
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
        (plotLines, l') = splitAt 12 l
        plot = fillPlot 24 12 plotLines
        fillPlot width height p = take height . map (fillLines width) $ p ++ repeat ""
        fillLines len line = take len $ line ++ repeat ' '
        (sepLine, rest) = splitAt 1 l'
        (linkMask, links) = parseSepLine sepLine
        parseSepLine [] = (fillPlot 24 12 [], Map.empty)
        parseSepLine [x]
            | (strip x) == "-" = (plot, makeLinkMap rest)
            | otherwise = (fillPlot 24 12 maskLines, makeLinkMap linkLines)
                where (maskLines, linkLines) = splitAt 12 rest
        makeLinkMap = Map.fromList . map parseLinkLine . filter (/= "")
        parseLinkLine line = (head begin, url)
            where (begin, _sp:url) = break isSpace line
        strip = dropWhile isSpace . dropWhileEnd isSpace

empty :: Parcel
empty = fromText Nothing "0 0"

toJSON :: Parcel -> JSON.JSValue
toJSON (Parcel owner pos@(x, y) art linkmask links) = jnfo [
    ("owner", case owner of
        Just name -> jnfs name
        Nothing -> JSON.JSNull),
    ("x", jnfn x),
    ("y", jnfn y),
    ("art", jnfa (map jnfs art)),
    ("linkmask", jnfa (map jnfs linkmask)),
    ("links", jnfo . map (\(k, v) -> ([k], jnfs v)) . Map.toList $ links)]
    where 
        jnfs = JSON.JSString . JSON.toJSString
        jnfn = JSON.JSRational False . toRational
        jnfo = JSON.JSObject . JSON.toJSObject
        jnfa = JSON.JSArray
--


fromJSON :: JSON.JSValue -> Parcel
fromJSON j = empty


hasOwner :: Parcel -> Bool
hasOwner parcel = case owner parcel of
                       Just _ -> True
                       Nothing -> False

