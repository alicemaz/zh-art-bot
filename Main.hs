{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Control.Monad.Random
import System.Directory

import Parse

mediaDir :: String
mediaDir = "paintings/"

-- the hundred bridges series is 10% of the corpus lol
weightFilename :: String -> (String, Rational)
weightFilename f
    | "yang-mingyi" `isPrefixOf` f = (f, 0.25)
    | otherwise = (f, 1)

main :: IO ()
main = do
    files <- listDirectory mediaDir
    file <- fromList $ weightFilename <$> files
    let Right parsed = parseFilename file

    putStrLn $ file ++ " / " ++ parsed
    return ()
