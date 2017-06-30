{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Monoid
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Monad.Random
import System.Directory
import Web.Twitter.Conduit

import Parse

mediaDir :: String
mediaDir = "paintings/"

mkCreds :: [BC.ByteString] -> TWInfo
mkCreds (key:sec:tok:toksec:[]) = setCredential keys tokens def
    where keys = twitterOAuth { oauthConsumerKey = key, oauthConsumerSecret = sec }
          tokens = Credential [("oauth_token", tok), ("oauth_token_secret", toksec)]
mkCreds _ = undefined

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

    creds <- (mkCreds . BC.lines) <$> BC.readFile ".credentials"
    mgr <- newManager tlsManagerSettings

    _ <- call creds mgr $ updateWithMedia (T.pack parsed) (MediaFromFile (mediaDir <> file))
    return ()
