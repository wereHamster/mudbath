{-# LANGUAGE OverloadedStrings #-}

module Executor
    ( buildDirectory
    , setupScript
    ) where


import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

import           Control.Applicative
import           Control.Arrow

import           System.Random

import           Prelude



randomString :: RandomGen d => Int -> d -> (String, d)
randomString len =
    first (Data.List.map toChar) . sequence' (Data.List.replicate len (randomR (0, 61)))
  where
    sequence' [] g = ([], g)
    sequence' (f:fs) g =
        let (f', g') = f g
            (fs', g'') = sequence' fs g'
         in (f' : fs', g'')
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52


generateRandomString :: IO Text
generateRandomString = do
    stdgen <- newStdGen
    return $ T.pack $ fst $ randomString 20 stdgen


buildDirectory :: IO Text
buildDirectory = mappend "/tmp/mudbath/" <$> generateRandomString


setupScript :: Text -> Text -> Text -> Text -> String
setupScript cachePath buildPath url commit = T.unpack $ mconcat $ Data.List.intersperse ";"
    [ "set -e"
    , "if test -d " <> cachePath
    , "then cd " <> cachePath <> " && git fetch --all --quiet"
    , "else git clone --mirror --quiet " <> url <> " " <> cachePath
    , "fi"
    , "rm -rf " <> buildPath
    , "git clone --quiet --reference " <> cachePath <> " " <> url <> " " <> buildPath
    , "cd " <> buildPath
    , "git checkout -q " <> commit
    ]
