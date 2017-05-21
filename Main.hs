{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Semigroup
import Data.Maybe
import Control.Monad
import Control.Monad.Random
import System.Directory
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

-- the hundred bridges series is 10% of the corpus lol
weightFilename :: String -> (String, Rational)
weightFilename f
    | "yang-mingyi" `isPrefixOf` f = (f, 0.25)
    | otherwise = (f, 1)

main :: IO ()
main = do
    files <- listDirectory "paintings"
    file <- fromList $ weightFilename <$> files
    let Right parsed = parseFilename file

    putStrLn $ file ++ " / " ++ parsed
    return ()
