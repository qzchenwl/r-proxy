{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Replace where

import qualified Prelude
import ClassyPrelude
import Data.Attoparsec.Text (Parser, parseOnly, takeWhile1, decimal, char, endOfInput)
import Text.Regex.PCRE.Heavy (Regex, scan)
import Safe (atMay)

type Replace = [Text] -> Text
type Source = Text
type Target = Text

instance IsString Replace where
    fromString = parseReplace . pack

parseReplace :: Text -> Replace
parseReplace t = case parseOnly (replacement <* endOfInput) t of
                   Left _ -> const t
                   Right r -> r

raw :: Parser Replace
raw = const <$> takeWhile1 (/= '$')

dollarGroup :: Parser Replace
dollarGroup = char '$' *> (grp <|> escaped)
    where curly = char '{' *> decimal <* char '}'
          grp = (\i ms -> fold (atMay ms i)) <$> (decimal <|> curly)
          escaped = const . singleton <$> char '$'

replacement :: Parser Replace
replacement = concat <$> many (dollarGroup <|> raw)

replace :: Source -> Regex -> Target -> Text
replace source regex target = (parseReplace target) matches
    where matches = case scan regex source of
            [(x, xs)] -> x:xs
            _ -> []
