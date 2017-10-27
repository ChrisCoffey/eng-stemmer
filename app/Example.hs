module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

import Data.Text.Stemming.English (stem)

main :: IO ()
main = do
    let stopwords = S.empty
    source <- fmap (stem stopwords) . T.lines <$> TIO.readFile "resources/voc.txt"
    validation <- T.lines <$> TIO.readFile "resources/output.txt"
    let errors = foldl validateStemming [] $ source `zip` validation
    print $ "Failed to stem: " ++ (show $ length errors)
    where
        validateStemming acc (stemmed, validation)
            | stemmed == validation = acc
            | otherwise = (stemmed, validation):acc



