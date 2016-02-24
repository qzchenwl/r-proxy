{-# LANGUAGE ViewPatterns #-}
module Utils where

import qualified Prelude
import ClassyPrelude
import Data.Text.Read (decimal)

readInt :: Text -> Maybe Int
readInt (decimal -> Right (i, _)) = Just i
readInt _ = Nothing

update :: (Eq k) => (k, v) -> [(k, v)] -> [(k, v)]
update (k, v) l = (k, v) : filter (\(k', _) -> k' /= k) l

applyAll (f:fs) a = applyAll fs (f a)
applyAll [] a = a

