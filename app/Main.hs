{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char                  (toUpper)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           System.Environment         (getArgs)
import           Text.Shakespeare.Text      (st)
import           Text.StrToHex

data Encoding
    = UTF8
    | UTF16LE
    | UTF16BE
    | UTF32LE
    | UTF32BE
    deriving (Show, Read)

encodingFromString :: String -> Encoding
encodingFromString = read . map toUpper

withText :: Encoding -> T.Text -> IO ()
withText enc t = B.putStrLn $ case enc of
    UTF8 -> strToHexUtf8 t
    UTF16LE -> strToHexUtf16LE t
    UTF16BE -> strToHexUtf16BE t
    UTF32LE -> strToHexUtf32LE t
    UTF32BE -> strToHexUtf32BE t

withStdin :: Encoding -> IO ()
withStdin enc = TIO.getContents >>= withText enc

displayHelp :: IO ()
displayHelp = TIO.putStrLn [st|Usage: str2hex ENCODING [TEXT | -]

Available encodings:
    * utf8
    * utf16le
    * utf16be
    * utf32le
    * utf32be
|]

main :: IO ()
main = getArgs >>= go
  where
    go [encoding] = withStdin $ encodingFromString encoding
    go [encoding, "-"] = withStdin $ encodingFromString encoding
    go [encoding, text] = withText (encodingFromString encoding) $ T.pack text
    go _ = displayHelp

