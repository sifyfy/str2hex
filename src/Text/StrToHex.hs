module Text.StrToHex
    ( strToHexUtf8
    , strToHexUtf16LE
    , strToHexUtf16BE
    , strToHexUtf32LE
    , strToHexUtf32BE
    ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE

strToHex :: (LT.Text -> LB.ByteString) -> Text -> LB.ByteString
strToHex encode = BB.toLazyByteString . LB.foldr (\w8 b -> BB.word8HexFixed w8 <> b) mempty . encode . LT.fromStrict

strToHexUtf8 :: Text -> LB.ByteString
strToHexUtf8 = strToHex LTE.encodeUtf8

strToHexUtf16LE :: Text -> LB.ByteString
strToHexUtf16LE = strToHex LTE.encodeUtf16LE

strToHexUtf16BE :: Text -> LB.ByteString
strToHexUtf16BE = strToHex LTE.encodeUtf16BE

strToHexUtf32LE :: Text -> LB.ByteString
strToHexUtf32LE = strToHex LTE.encodeUtf32LE

strToHexUtf32BE :: Text -> LB.ByteString
strToHexUtf32BE = strToHex LTE.encodeUtf32BE

