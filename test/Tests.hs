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
    ("constant","constant")
    ]
