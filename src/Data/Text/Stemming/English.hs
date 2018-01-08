{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Text.Stemming.English
(
    stem
) where

import Control.Monad.Reader (MonadReader)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Foldable (asum)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Stemming.Constants.English as Constants

import Debug.Trace

-- $setup
-- Always import Data.Text and other modules
-- >>> import Data.Text

stem :: S.Set T.Text -> T.Text -> T.Text
stem stopwords input =
    fromMaybe stemmed $ checkStop stopwords word <|>
        checkLength word <|>
        checkSpecialWords word
    where
    word = T.toLower input
    stemmed = doStem word

data WordRegion = WR {
    word :: T.Text,
    r1 :: T.Text,
    r2 :: T.Text
    } deriving Show

mapWR :: (T.Text -> T.Text) -> WordRegion -> WordRegion
mapWR f (WR word r1 r2) = WR (f word) (f r1) (f r2)

vowls :: [Char]
vowls = ['a', 'e', 'i', 'o', 'u', 'y']

-- | The main stemming function. The Porter2 algorithm relies on
-- iteratively applying a number of rules in sequence to gradually transform
-- the input string
doStem :: T.Text -> T.Text
doStem =
    extractResult . stemmingSteps . regions
    where
        extractResult = T.toLower
        stemmingSteps = step5 . step4 . word . step3 . step2 . step1 . step0
        regions = computeRegions . scrubWord

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
-- >>> let wr = computeRegions $ scrubWord (pack "beauty")
-- >>> word $  step0 wr
-- "beauty"
--
-- >>> let wr2 = computeRegions $ scrubWord (pack "beauty's")
-- >>> word $  step0 wr2
-- "beauty"
step0 ::
    WordRegion ->
    WordRegion
step0 wr = fromMaybe wr . asum $ dropSuffix wr <$> step0Suffixes
    where
    dropSuffix wr s
        | T.isSuffixOf s (word wr) = let
            len = T.length s
            wr' = mapWR (T.dropEnd len) wr
            in Just wr'
        | otherwise = Nothing


step1 ::
    WordRegion ->
    WordRegion
step1 = step1c . step1b . step1a

-- | Replaces a few suffixes
--
-- >>> let wr = computeRegions $ scrubWord (pack "misses")
-- >>> word $  step1a wr
-- "miss"
--
-- >>> let wr = computeRegions $ scrubWord (pack "tied")
-- >>> word $  step1a wr
-- "tie"
--
-- >>> let wr = computeRegions $ scrubWord (pack "cries")
-- >>> word $  step1a wr
-- "cri"
--
-- >>> let wr = computeRegions $ scrubWord (pack "gas")
-- >>> word $  step1a wr
-- "gas"
--
-- >>> let wr = computeRegions $ scrubWord (pack "gaps")
-- >>> word $  step1a wr
-- "gap"
step1a ::
    WordRegion ->
    WordRegion
step1a wr =
    let w = word wr
    in fromMaybe wr . asum $ [(f . const wr) <$> T.stripSuffix s w | (s, f) <- suffixes]
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
        | T.any (`elem` vowls) (T.dropEnd 2 (word wr)) &&
          T.takeEnd 2 (word wr) `notElem` ["ss", "us"]
            = mapWR (T.dropEnd 1) wr
        | otherwise = wr

-- | Replace ed, ly, & ing variant suffixes
--
-- >>> let wr = computeRegions $ scrubWord (pack "speed")
-- >>> word $  step1b wr
-- "sp"
--
-- >>> let wr = computeRegions $ scrubWord (pack "luxuriating")
-- >>> word $  step1b wr
-- "luxuriate"
--
-- >>> let wr = computeRegions $ scrubWord (pack "hopped")
-- >>> word $  step1b wr
-- "hop"
--
-- >>> let wr = computeRegions $ scrubWord (pack "hoped")
-- >>> word $  step1b wr
-- "hope"
--
-- >>> wr = computeRegions $ scrubWord (pack "heeded")
-- >>> word $ step1b wr
-- "heed"
step1b ::
    WordRegion ->
    WordRegion
step1b wr =
    let w' = word wr
    in fromMaybe wr . asum $ [(f . const wr) <$> T.stripSuffix s w' | (s,f) <- suffixes]
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
                if
                | T.takeEnd 2 w' `elem` doubleConsonants -> endsInDoubleConsonant wr'
                | T.null (r1 wr') && (shortV w' || twoLetterWord w') -> addAnE wr'
                | T.length w' <= 3 -> addAnE wr'
                | T.takeEnd 2 w' `elem` specialEndings -> addAnE wr'
                | otherwise -> wr'
            _ -> wr
        where
            twoLetterWord w = T.length w == 2 && T.head w `elem` vowls && T.last w `notElem` vowls
            wr' = mapWR (T.dropEnd (T.length s)) wr
            doubleConsonants = ["bb", "dd", "ff", "gg", "mm", "nn", "pp", "rr", "tt"]
            endsInDoubleConsonant = mapWR (T.dropEnd 1)
            addAnE = mapWR (\w -> if T.null w then w else w `T.snoc` 'e')
            specialEndings = ["at", "bl", "iz"]

-- | Replace trailing 'y' with an i where appropriate
--
-- >>> let wr = computeRegions $ scrubWord (pack "cry")
-- >>> word $  step1c wr
-- "cri"
--
-- >>> let wr = computeRegions $ scrubWord (pack "by")
-- >>> word $  step1c wr
-- "by"
--
-- >>> let wr = computeRegions $ scrubWord (pack "say")
-- >>> word $  step1c wr
-- "saY"
step1c ::
    WordRegion ->
    WordRegion
step1c = swapY
    where
    endsWith w c = T.takeEnd 1 w == c
    safeNonVowl = fromMaybe '_' . safeHead
    swapY wr
        | T.length (word wr) <= 2 = wr
        | word wr `endsWith` "y" && notElem (safeNonVowl . T.takeEnd 1 . T.init $ word wr) vowls =
            mapWR (maybe "" (`T.snoc` 'i') . T.stripSuffix "y") wr
        | word wr `endsWith` "Y" && notElem (safeNonVowl . T.takeEnd 1 . T.init $ word wr) vowls =
            mapWR (maybe "" (`T.snoc` 'i') . T.stripSuffix "Y") wr
        | otherwise = wr

safeHead ::
    T.Text ->
    Maybe Char
safeHead t
    | T.length t >= 1 = Just $ T.head t
    | otherwise = Nothing

swapLastWithE ::
    T.Text ->
    T.Text
swapLastWithE = (`T.snoc` 'e') . T.dropEnd 1

-- | Replace larger suffixes in order from largest to smallest, iff the suffix is in R1
step2 ::
    WordRegion ->
    WordRegion
step2 wr =
    let w = word wr
    in fromMaybe wr . asum $
        [const (suffixInR1 s f wr) =<< T.stripSuffix s w | (s, f) <- suffixes]
    where
    suffixInR1 :: T.Text -> (T.Text -> T.Text) -> WordRegion -> Maybe WordRegion
    suffixInR1 s f wr  = fmap (const $ mapWR f wr) . T.stripSuffix s $ r1 wr
    suffixes = [
        ("ational", swapLastWithE . T.dropEnd 4),
        ("fulness", T.dropEnd 4),
        ("ousness", T.dropEnd 4),
        ("iveness", T.dropEnd 4),
        ("tional", T.dropEnd 2),
        ("biliti", (`T.append` "le") . T.dropEnd 6),
        ("lessli", T.dropEnd 2),
        ("iviti", swapLastWithE . T.dropEnd 2),
        ("entli", T.dropEnd 2),
        ("ation", swapLastWithE . T.dropEnd 2),
        ("alism", T.dropEnd 3),
        ("aliti", T.dropEnd 3),
        ("ousli", T.dropEnd 2),
        ("fulli", T.dropEnd 2),
        ("enci", swapLastWithE),
        ("alli", T.dropEnd 2),
        ("anci", swapLastWithE),
        ("abli", swapLastWithE),
        ("izer", T.dropEnd 1),
        ("ator", swapLastWithE . T.dropEnd 1),
        ("logi", T.dropEnd 1),
        ("bli", swapLastWithE),
        ("li", \w -> if (fromMaybe ' ' . safeHead $ T.takeEnd 3 w) `elem` liEnding
                     then T.dropEnd 2 w
                     else w)
        ]
    liEnding :: String
    liEnding = "cdeghkmnrt"

suffixInR1 :: T.Text -> (T.Text -> T.Text) -> WordRegion -> Maybe WordRegion
suffixInR1 s f wr  = fmap (const $ mapWR f wr) . T.stripSuffix s $ r1 wr

-- TODO this is identical to suffixInR1
suffixInR2 ::
    T.Text ->
    (T.Text -> T.Text) ->
    WordRegion ->
    Maybe WordRegion
suffixInR2 s f wr
    | T.isSuffixOf s (r2 wr)
        = Just $ mapWR f wr
    | otherwise
        = Nothing

-- | Search for the longest suffix perform the replacement iff the suffix is in R1 as well
step3 ::
    WordRegion ->
    WordRegion
step3 wr =
    let w = word wr
    in fromMaybe wr . asum $
        specialCase wr:[const (suffixInR1 s f wr) =<< T.stripSuffix s w | (s, f) <- suffixes]
    where
    suffixes = [
        ("ational", swapLastWithE . T.dropEnd 4),
        ("tional", T.dropEnd 2),
        ("alize", T.dropEnd 3),
        ("icate", T.dropEnd 3),
        ("iciti", T.dropEnd 3),
        ("ical", T.dropEnd 2),
        ("ness", T.dropEnd 4),
        ("ful", T.dropEnd 3)
        ]
    specialCase wr = const (suffixInR2 "ative" id wr) =<< T.stripSuffix "ative" (word wr)

-- | Deletes many common suffixes
-- >>> let wr = computeRegions $ scrubWord (pack "conscious")
-- >>> word $ step4 wr
-- "consci"
step4 ::
    T.Text ->
    T.Text
step4 w =
    maybe w word . asum $
        [const (suffixInR2 s f wr) =<< T.stripSuffix s w | (s, f) <- suffixes]
    where
    wr = computeRegions w
    ionHandler wr = let
        w = word wr
        in if T.isSuffixOf "sion" w || T.isSuffixOf "tion" w
           then T.dropEnd 3
           else id
    suffixes = [
        ("ement", T.dropEnd 5),
        ("ment", T.dropEnd 4),
        ("ance", T.dropEnd 4),
        ("ence", T.dropEnd 4),
        ("able", T.dropEnd 4),
        ("ible", T.dropEnd 4),
        ("ion", ionHandler wr),
        ("ant", T.dropEnd 3),
        ("ent", T.dropEnd 3),
        ("ism", T.dropEnd 3),
        ("ate", T.dropEnd 3),
        ("iti", T.dropEnd 3),
        ("ous", T.dropEnd 3),
        ("ive", T.dropEnd 3),
        ("ize", T.dropEnd 3),
        ("al", T.dropEnd 2),
        ("er", T.dropEnd 2),
        ("ic", T.dropEnd 2)
        ]

step5 ::
    T.Text ->
    T.Text
step5 w = maybe w word . asum $ [stripL, stripE] <*> [wr]
    where
    wr = computeRegions w
    stripL wr
        | T.isSuffixOf "l" (r2 wr) && T.isSuffixOf "ll" (word wr)
            = Just $ mapWR (T.dropEnd 1) wr
        | otherwise = Nothing
    stripE wr
        | T.isSuffixOf "e" (r2 wr)
            = Just $ mapWR (T.dropEnd 1) wr
        | T.isSuffixOf "e" (r1 wr) && not (shortV (word wr))
            = Just $ mapWR (T.dropEnd 1) wr
        | otherwise
            = Just wr

vWXY = vowls <> ['w', 'x', 'Y']

shortV ::
    T.Text ->
    Bool
shortV w = T.length w >= 4 &&
    (
        T.head (T.takeEnd 4 w) `notElem` vWXY &&
        T.head (T.takeEnd 3 w) `elem` vowls &&
        T.head (T.takeEnd 2 w) `notElem` vowls
    )


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
checkSpecialWords word = M.lookup word Constants.specialStems
