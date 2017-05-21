{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Semigroup
import Data.Maybe
import Control.Monad
import System.Random
import System.Directory
import Text.Parsec

name = unwords <$> sepBy1 (many1 lower) (char '-')

filename = do
    artist <- name
    char '_'
    title <- try name <|> string "20.8.84"
    mSuffix <- optionMaybe $ char '_' >> (try name <|> many1 alphaNum)
    let suffix = join $ maybeToList $ (\s -> " (" <> s <> ")") <$> mSuffix
    string ".jpg"
    return (artist <> " - " <> title <> suffix)

parseFilename f = parse filename f f

main :: IO ()
main = do
    files <- listDirectory "paintings"
    file <- (files !!) <$> randomRIO (0, length files - 1)
    let Right parsed = parseFilename file

    putStrLn $ file ++ " / " ++ parsed
    return ()
