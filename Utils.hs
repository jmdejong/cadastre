
{-# LANGUAGE OverloadedStrings #-}

module Utils where


import Crypto.Hash
import System.IO.Unsafe
import qualified Data.Text as T

import Data.List

type Pos = (Int, Int)

assume :: Maybe a -> a
assume (Just x) = x

def :: a -> Maybe a -> a
def _ (Just x) = x
def d Nothing = d

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


