{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Stemming.English

import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Set as S

main :: IO ()
main = defaultMain stemmerTests

stemmerTests :: TestTree
stemmerTests = testGroup "Stemmer Tests" porterTests

porterTests :: [TestTree]
porterTests =
    porterTest <$> testCases
    where
        noStopWords = S.empty
        porterTest (initial, stemmed) =
            testCase (T.unpack initial <> " -> " <> T.unpack stemmed) $
                stem noStopWords initial @?= stemmed


testCases :: [(T.Text, T.Text)]
testCases = [
    ("consign","consign"),
    ("consigned","consign"),
    ("consigning","consign"),
    ("consignment","consign"),
    ("consist","consist"),
    ("consisted","consist"),
    ("consistency","consist"),
    ("consistent","consist"),
    ("consistently","consist"),
    ("consisting","consist"),
    ("consists","consist"),
    ("consolation","consol"),
    ("consolations","consol"),
    ("consolatory","consolatori"),
    ("console","consol"),
    ("consoled","consol"),
    ("consoles","consol"),
    ("consolidate","consolid"),
    ("consolidated","consolid"),
    ("consolidating","consolid"),
    ("consoling","consol"),
    ("consolingly","consol"),
    ("consols","consol"),
    ("consonant","conson"),
    ("consort","consort"),
    ("consorted","consort"),
    ("consorting","consort"),
    ("conspicuous","conspicu"),
    ("conspicuously","conspicu"),
    ("conspiracy","conspiraci"),
    ("conspirator","conspir"),
    ("conspirators","conspir"),
    ("conspire","conspir"),
    ("conspired","conspir"),
    ("conspiring","conspir"),
    ("constable","constabl"),
    ("constables","constabl"),
    ("constance","constanc"),
    ("constancy","constanc"),
    ("constant","constant"),
    ("knack","knack"),
    ("knackeries","knackeri"),
    ("knacks","knack"),
    ("knag","knag"),
    ("knave","knave"),
    ("knaves","knave"),
    ("knavish","knavish"),
    ("kneaded","knead"),
    ("kneading","knead"),
    ("knee","knee"),
    ("kneel","kneel"),
    ("kneeled","kneel"),
    ("kneeling","kneel"),
    ("kneels","kneel"),
    ("knees","knee"),
    ("knell","knell"),
    ("knelt","knelt"),
    ("knew","knew"),
    ("knick","knick"),
    ("knif","knif"),
    ("knife","knife"),
    ("knight","knight"),
    ("knightly","knight"),
    ("knights","knight"),
    ("knit","knit"),
    ("knits","knit"),
    ("knitted","knit"),
    ("knitting","knit"),
    ("knives","knive"),
    ("knob","knob"),
    ("knobs","knob"),
    ("knock","knock"),
    ("knocked","knock"),
    ("knocker","knocker"),
    ("knockers","knocker"),
    ("knocking","knock"),
    ("knocks","knock"),
    ("knopp","knopp"),
    ("knot","knot"),
    ("knots","knot"),
    ("cheating", "cheat"),
    ("floated", "float"),
    ("migrated", "migrat"),
    ("choked", "choke"),
    ("foxes", "fox"),
    ("bribing", "bribe")
    ]

