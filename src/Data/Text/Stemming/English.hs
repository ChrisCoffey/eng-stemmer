{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Text.Stemming.English
(   stem,
    pureStem
) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Foldable (asum)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T

--todo pull out the state monad, I don't need it

-- $setup
-- Always import Data.Text and other modules
-- >>> import Data.Text

data WordRegion = WR {
    word :: T.Text,
    r1 :: T.Text,
    r2 :: T.Text
    } deriving Show

mapWR :: (T.Text -> T.Text) -> WordRegion -> WordRegion
mapWR f (WR word r1 r2) = WR (f word) (f r1) (f r2)

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

-- | The main stemming function. The Porter2 algorithm relies on
-- mutation and is much simpler to implement in State vs. a fold
doStem :: T.Text -> T.Text
doStem input = word . flip execState regions $
    step0 >> step1 >> step2 >> step3 >> step4 >> step5
    where
    regions = computeRegions $ scrubWord input

scrubWord :: T.Text -> T.Text
scrubWord =
    markConsonantY . stripLeadingApostrophe . normApostrophe

--
-- Helper functions for word scrubbing
--

-- | Slowish approach to normalizing the apostophe character
--
-- >>> pack "isn't" == normApostrophe (pack "isn\x2018t")
-- True
-- >>> pack "isn't" == normApostrophe (pack "isn\x2019t")
-- True
-- >>> pack "isn't" == normApostrophe (pack "isn\x201Bt")
-- True
normApostrophe :: T.Text -> T.Text
normApostrophe =
    T.replace "\x2018" "\x27" .
    T.replace "\x2019" "\x27" .
    T.replace "\x201B" "\x27"


-- | Assumes the input is non-empty by virtue of `checkLength`
--
-- >>> pack "isn't"  == stripLeadingApostrophe (pack "'isn't")
-- True
stripLeadingApostrophe :: T.Text -> T.Text
stripLeadingApostrophe word
    | T.isPrefixOf "\x27" word = T.tail word
    | otherwise = word

-- | Encodes the rules for when 'y' behaves as a consonant via an uppercase 'Y'
--
-- >>> pack "Your" == markConsonantY (pack "your")
-- True
-- >>> pack "toY" == markConsonantY (pack "toy")
-- True
-- >>> pack "bouYant" == markConsonantY (pack "bouyant")
-- True
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

--
-- Regions
--

-- | Vowl regions are computed from the front of the word. Examples pulled from:
-- https://snowballstem.org/texts/r1r2.html
--
-- >>> r1 (computeRegions $ pack "beautiful") == pack "iful"
-- True
-- >>> r2 (computeRegions $ pack "beautiful") == pack "ul"
-- True
--
-- >>> r1 (computeRegions $ pack "beauty") == pack "y"
-- True
-- >>> r2 (computeRegions $ pack "beauty") == pack ""
-- True
--
-- >>> r1 (computeRegions $ pack "beau") == pack ""
-- True
-- >>> r2 (computeRegions $ pack "beau") == pack ""
-- True
--
-- >>> r1 (computeRegions $ pack "animadversion") == pack "imadversion"
-- True
-- >>> r2 (computeRegions $ pack "animadversion") == pack "adversion"
-- True

-- >>> r1 (computeRegions $ pack "sprinkled") == pack "kled"
-- True
-- >>> r2 (computeRegions $ pack "sprinkled") == pack ""
-- True

-- >>> r1 (computeRegions $ pack "eucharist") == pack "harist"
-- True
-- >>> r2 (computeRegions $ pack "eucharist") == pack "ist"
-- True
computeRegions :: T.Text -> WordRegion
computeRegions word = let
    r1 = fromMaybe (region word) (specialRegion word) -- Attempt to extract a special region, but otherwise normal region
    r2 = region r1
    in WR word r1 r2
    where
    region txt
        | T.null txt || T.null (T.tail txt) = ""
        | otherwise =
            case (T.head txt, T.head (T.tail txt)) of
                (a,b) | a `elem` vowls && (b `notElem` vowls) -> T.tail $ T.tail txt
                (a,b) -> region $ T.tail txt
    specialRegion txt = listToMaybe . catMaybes $ [T.stripPrefix x txt | x <- ["gener", "commun", "arsen"]]

--
-- Steps
--

-- | These suffixes are by length. This is used by Step0 to check for a suffix by length
step0Suffixes :: [T.Text]
step0Suffixes = ["'s'", "'s", "'"]

-- | Removes posessive suffixes
--
-- >>> import Control.Monad.State.Strict
-- >>> let wr = computeRegions $ scrubWord (pack "beauty")
-- >>> word $ execState step0 wr
-- "beauty"
--
-- >>> let wr2 = computeRegions $ scrubWord (pack "beauty's")
-- >>> word $ execState step0 wr2
-- "beauty"
step0 :: State WordRegion ()
step0 = do
    wr <- get
    let wr' = fromMaybe wr . asum $ dropSuffix wr <$> step0Suffixes
    put wr'
    where
    dropSuffix wr s
        | T.isSuffixOf s (word wr) = let
            len = T.length s
            wr' = mapWR (T.dropEnd len) wr
            in Just wr'
        | otherwise = Nothing


step1 :: State WordRegion ()
step1 = undefined

-- | Replaces a few suffixes
--
-- >>> import Control.Monad.State.Strict
-- >>> let wr = computeRegions $ scrubWord (pack "misses")
-- >>> word $ execState step1a wr
-- "miss"
--
-- >>> let wr = computeRegions $ scrubWord (pack "tied")
-- >>> word $ execState step1a wr
-- "tie"
--
-- >>> let wr = computeRegions $ scrubWord (pack "cries")
-- >>> word $ execState step1a wr
-- "cri"
--
-- >>> let wr = computeRegions $ scrubWord (pack "gas")
-- >>> word $ execState step1a wr
-- "gas"
--
-- >>> let wr = computeRegions $ scrubWord (pack "gaps")
-- >>> word $ execState step1a wr
-- "gap"
step1a :: State WordRegion ()
step1a = do
    wr <- get
    let w = word wr
        wr' = fromMaybe wr . asum $ [(f . const wr) <$> T.stripSuffix s w | (s, f) <- suffixes]
    put wr'
    where
    suffixes = [
        ("sses", ssesF),
        ("ied", ieF),
        ("ies", ieF),
        ("s", sF)]
    -- Replace 'sses' with 'ss'
    ssesF = mapWR (T.dropEnd 2)
    -- Replace 'ied' or 'ies' with 'i' or 'ie'
    ieF wr
        | T.length (word wr) > 4 = mapWR (T.dropEnd 2) wr
        | otherwise = mapWR (T.dropEnd 1) wr
    -- Drop trailing 's' iff there is a vowl prior to the 2nd to last character
    sF wr
        | T.any (`elem` vowls) (T.dropEnd 2 (word wr)) = mapWR (T.dropEnd 1) wr
        | otherwise = wr

step1b :: State WordRegion ()
step1b = do
    wr <- get
    let w' = word wr
        wr' = fromMaybe wr . asum $ [(f . const wr) <$> T.stripSuffix s w' | (s,f) <- suffixes]
    put wr'
    where
    suffixes = [
        ("eedly", eeF "eedly"),
        ("eed", eeF "eed"),
        ("edly", edF "edly"),
        ("ingly", edF "ingly"),
        ("ed", edF "ed"),
        ("ing", edF "ing")
        ]

    -- Replace 'eed' variants with 'ee'
    eeF s wr = case mapWR (fromMaybe "" . T.stripSuffix s) wr of
        -- We know by virtue of r1 being nonempty that it ended in the suffix
        (WR word r1 r2) | not (T.null r1) -> let
            r2' = if T.null r2
                  then ""
                  else T.append r2 "ee"
            in WR (T.append word "ee") (T.append r1 "ee") r2'
        _ -> mapWR (T.dropEnd $ T.length s) wr

    -- Remove 'ed' & "ing" variants & follow byzantine replace rules...
    edF s wr =
        case T.stripSuffix s (word wr) of
            Just w' | T.any (`elem` vowls) w' ->
                let wr' = mapWR (fromMaybe "" . T.stripSuffix s) wr
                in if
                    | T.takeEnd 2 w' `elem` doubleConsonants -> endsInDoubleConsonant wr'
                    | T.length w' <= 3 -> addAnE wr'
                    | T.takeEnd 2 w' `elem` specialEndings -> addAnE wr'
                    | otherwise -> wr'
            _ -> wr
        where
        doubleConsonants = ["bb", "dd", "ff", "gg", "mm", "nn", "pp", "rr", "tt"]
        endsInDoubleConsonant = mapWR (T.dropEnd 1)
        addAnE = mapWR (\w -> if T.null w then w else w `T.snoc` 'e')
        specialEndings = ["at", "bl", "iz"]

step2 :: State WordRegion ()
step2 = undefined


step3 :: State WordRegion ()
step3 = undefined

step4 :: State WordRegion ()
step4 = undefined

step5 :: State WordRegion ()
step5 = undefined
--
-- Initial checks. These short circut for various special cases
--

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
