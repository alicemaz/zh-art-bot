{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parse (parseFilename) where

import Data.Semigroup
import Data.Maybe
import Control.Monad
import Text.Parsec

name = unwords <$> sepBy1 (many1 lower) (char '-')

filename = do
    artist <- name
    _ <- char '_'
    title <- try name <|> string "20.8.84"
    mSuffix <- optionMaybe $ char '_' >> (try name <|> many1 alphaNum)
    let suffix = join $ maybeToList $ (\s -> " (" <> s <> ")") <$> mSuffix
    _ <- string ".jpg"
    return (artist <> " - " <> title <> suffix)

parseFilename f = parse filename f f
