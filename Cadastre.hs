module Cadastre (
    Cadastre,
    empty,
    fromTexts,
    toJSON,
    fromJSON
) where
--
import qualified Parcel
import qualified Text.JSON as JSON

data Cadastre = Cadastre [Parcel.Parcel] Integer


empty :: Cadastre
empty = Cadastre [] 0

fromTexts :: Integer -> [(Maybe String, String)] -> Cadastre
fromTexts seed texts = Cadastre (map (\(owner, text) -> Parcel.fromText owner text) texts) seed

-- toJSON :: Real a => a Cadastre -> JSON.JSValue
-- toJSON cadastre = 

toJSON :: Cadastre -> JSON.JSValue
toJSON (Cadastre parcels seed) = JSON.JSObject . JSON.toJSObject $ [
    ("places", parcelsToJSONObject parcels),
    ("seed", JSON.JSRational False . toRational $ seed),
    ("owners", JSON.JSObject . JSON.toJSObject . map makeOwnerTuple . filter Parcel.hasOwner $ parcels)]
    where
        makeOwnerTuple (Parcel.Parcel (Just owner) pos _ _ _) = (owner, JSON.JSString . JSON.toJSString . hashPos $ pos)
        parcelsToJSONObject = jnfo . map prepareParcel
        prepareParcel parcel = (hashPos (Parcel.location parcel), Parcel.toJSON parcel)
        jnfs = JSON.JSString . JSON.toJSString
        jnfn = JSON.JSRational False . toRational
        jnfo = JSON.JSObject . JSON.toJSObject
        jnfa = JSON.JSArray

fromJSON :: JSON.JSValue -> Cadastre
fromJSON _ = empty


-- parcelsToJSONObject :: [Parcel.Parcel] -> JSON.JSValue
-- parcelsToJSONObject = jnfo . map prepareParcel
--     where
--         prepareParcel parcel = (hashPos (Parcel.location parcel), Parcel.toJSON parcel)
--         jnfs = JSON.JSString . JSON.toJSString
--         jnfn = JSON.JSRational False . toRational
--         jnfo = JSON.JSObject . JSON.toJSObject
--         jnfa = JSON.JSArray


hashPos :: (Int, Int) -> String
hashPos (x, y) = (show x) ++ "," ++ (show y)
