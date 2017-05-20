{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Semigroup
import Data.Maybe
import Control.Monad
import System.Directory
import Text.Parsec

artistName = do
    name <- many1 $ noneOf "-_."
    given <- (join . maybeToList . fmap (' ':)) <$> optionMaybe (char '-' >> artistName)
    return (name <> given)

tweetText f = parse artistName f f

main :: IO ()
main = do
    filenames <- listDirectory "paintings"
    --let Right testy = sequence $ tweetText <$> filenames
    --putStrLn $ concatMap (++ "\n") $ nub testy
    print $ tweetText <$> filenames
    return ()
