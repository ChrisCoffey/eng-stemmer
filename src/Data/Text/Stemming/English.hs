{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Stemming.English
(   stem,
    pureStem
) where

import Control.Monad.Reader (MonadReader)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T

class HasStemmingConf a where
    stopwords :: a -> S.Set T.Text

-- | 'stem' simply wraps 'pureStem' with a Reader monad
stem :: (Monad m, MonadReader r m, HasStemmingConf r) =>
    T.Text ->
    m T.Text
stem = undefined

vowls :: [Char]
vowls = ['a', 'e', 'i', 'o', 'u', 'y']

pureStem :: S.Set T.Text -> T.Text -> T.Text
pureStem stopwords input =
    fromMaybe stemmed $ checkStop stopwords word <|>
        checkLength word <|>
        checkSpecialWords word
    where
    word = T.toLower input
    stemmed = doStem word

-- | The main stemming function
doStem :: T.Text -> T.Text
doStem = undefined

scrubWord :: T.Text -> T.Text
scrubWord =
    markConsonantY . stripLeadingApostrophe . normApostrophe

-- | Slowish approach to normalizing the apostophe character
normApostrophe :: T.Text -> T.Text
normApostrophe =
    T.replace "\x2018" "\x27" .
    T.replace "\x2018" "\x27" .
    T.replace "\x201B" "\x27"

-- | Assumes the input is non-empty by virtue of `checkLength`
stripLeadingApostrophe :: T.Text -> T.Text
stripLeadingApostrophe word
    | T.isPrefixOf "\x27" word = T.tail word
    | otherwise = word

-- | Encodes the rules for when 'y' behaves as a consonant via an uppercase 'Y'
markConsonantY :: T.Text -> T.Text
markConsonantY = go . replaceInit
    where
    replaceInit :: T.Text -> T.Text
    replaceInit w
        | T.head w == 'y' = 'Y' `T.cons` T.tail w
        | otherwise = w
    go w = T.head w `T.cons` T.zipWith f w (T.tail w)
    f c 'y'
        | c `elem` vowls = 'Y'
        | otherwise = 'y'
    f _ c = c


-- | Stopwords will not be stemmed
checkStop :: S.Set T.Text -> T.Text -> Maybe T.Text
checkStop sw word
    | word `S.member` sw = Just word
    | otherwise = Nothing

-- | No english words of 1 or 2 characters may be stemmed
checkLength :: T.Text -> Maybe T.Text
checkLength word
    | T.length word <= 2 = Just word
    | otherwise = Nothing

-- | There are a handful of irregularly stemmed words in English. This
-- automatically stems them since its not worth the effort to algorithmically
-- stem them.
checkSpecialWords :: T.Text -> Maybe T.Text
checkSpecialWords word = M.lookup word specialWords
    where
        specialWords = M.fromList [("skis" , "ski"),
            ("skies" , "sky"),
            ("dying" , "die"),
            ("lying" , "lie"),
            ("tying" , "tie"),
            ("idly" , "idl"),
            ("gently" , "gentl"),
            ("ugly" , "ugli"),
            ("early" , "earli"),
            ("only" , "onli"),
            ("singly" , "singl"),
            ("sky" , "sky"),
            ("news" , "news"),
            ("howe" , "howe"),
            ("atlas" , "atlas"),
            ("cosmos" , "cosmos"),
            ("bias" , "bias"),
            ("andes" , "andes"),
            ("inning" , "inning"),
            ("innings" , "inning"),
            ("outing" , "outing"),
            ("outings" , "outing"),
            ("canning" , "canning"),
            ("cannings" , "canning"),
            ("herring" , "herring"),
            ("herrings" , "herring"),
            ("earring" , "earring"),
            ("earrings" , "earring"),
            ("proceed" , "proceed"),
            ("proceeds" , "proceed"),
            ("proceeded" , "proceed"),
            ("proceeding" , "proceed"),
            ("exceed" , "exceed"),
            ("exceeds" , "exceed"),
            ("exceeded" , "exceed"),
            ("exceeding" , "exceed"),
            ("succeed" , "succeed"),
            ("succeeds" , "succeed"),
            ("succeeded" , "succeed"),
            ("succeeding" , "succeed")]
