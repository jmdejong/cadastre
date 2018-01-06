
{-# LANGUAGE OverloadedStrings #-}

module Utils where



-- import Data.ByteString.Char8 (pack)
-- import qualified Data.ByteArray as BA
-- import qualified Data.Binary as Bin
import Crypto.Hash
import System.IO.Unsafe
import qualified Data.Text as T

import Data.List

type Pos = (Int, Int)

assume :: Maybe a -> a
assume (Just x) = x

-- randomize :: (Show a, BA.ByteArrayAccess b) => a -> b --Bin.Word8
-- randomize = md5 . pack . show
--     where 
--         md5 x = hash x :: Digest MD5


htmlEscape :: T.Text -> T.Text
htmlEscape "" = ""
htmlEscape text = T.concatMap escape text
    where escape c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '\'' -> "&#x27;"
            '"' -> "&quot;"
            x -> T.singleton x


debugPrint :: Show a => a -> b -> b
debugPrint msg ret= unsafePerformIO $ do
    print msg
    return ret


