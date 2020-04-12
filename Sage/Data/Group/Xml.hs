
module Data.Group.Xml where

import Data.List
import Data.Maybe
import qualified Data.Group as Group
import Data.Group (Group)
import Utils

xml :: (Show a, Group a) => [a] -> String -> String
xml xs name = header ++ definition xs name

tag :: String -> String -> String
tag name body = '<' : name ++ ">\n" ++ body ++ "</" ++ name ++ ">\n"

table :: Group a => [a] -> String
table xs = tag "multtable" $ concatMap (row . list xs) (Group.cayleyTable xs)

row :: String -> String
row str = "<row> " ++ str ++ " </row>\n"

genList :: Group a => [a] -> String
genList xs = "<generators list=\"" ++ list xs (Group.minGen xs) ++ "\"/>\n"

list :: Group a => [a] -> [a] -> String
list xs = intercalate " " . map (show . index xs)

index :: Group a => [a] -> a -> Int
index = fromJust .: flip elemIndex

representation :: (Show a, Group a) => [a] -> String
representation xs = tag "representation" $ concatMap (element . show) xs

definition :: (Show a, Group a) => [a] -> String -> String
definition xs str = tag "group" $
    name str ++
    table xs ++
    genList xs ++
    representation xs

header = "<!DOCTYPE groupexplorerml>\n"

name :: String -> String
name name = "<name text=\"" ++ name ++ "\"><mrow><mi>" ++ name ++ "</mi></mrow></name>\n"

element :: String -> String
element name = "<element text=\"" ++ name ++ "\"><mi>" ++ name ++ "</mi></element>\n"
