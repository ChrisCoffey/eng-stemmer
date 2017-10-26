{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Stemming.Constants.English where

import qualified Data.Text as T
import qualified Data.Map as M

stopWords :: [T.Text]
stopWords = []

specialStems :: M.Map T.Text T.Text
specialStems = M.fromList
    [("skis" , "ski"),
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
    ("succeeding" , "succeed")
    ]
