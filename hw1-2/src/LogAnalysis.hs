{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LogAnalysis where

import Log
import Text.Read
import Data.Maybe

buildError :: [String] -> LogMessage
buildError (errLvl: timeStamp:messages) =
    if isNothing maybeErrLvl || isNothing maybeTimeStamp then
        Unknown (unwords ("E":errLvl:timeStamp:messages))
    else
        LogMessage (Error (fromJust maybeErrLvl)) (fromJust maybeTimeStamp) (unwords messages)
    where
        maybeErrLvl = readMaybe errLvl :: Maybe Int
        maybeTimeStamp = readMaybe timeStamp :: Maybe Int
buildError list =
    Unknown (unwords ("E":list))

buildInfo :: [String] -> LogMessage
buildInfo (timeStamp:messages) =
    if isNothing maybeTimeStamp then
        Unknown (unwords ("I":timeStamp:messages))
    else
        LogMessage Info (fromJust maybeTimeStamp) (unwords messages)
    where maybeTimeStamp = readMaybe timeStamp :: Maybe Int
buildInfo list =
    Unknown (unwords ("I":list))

buildWarning :: [String] -> LogMessage
buildWarning (timeStamp:messages) =
    if isNothing maybeTimeStamp then
        Unknown (unwords ("W":timeStamp:messages))
    else
        LogMessage Warning (fromJust maybeTimeStamp) (unwords messages)
    where maybeTimeStamp = readMaybe timeStamp :: Maybe Int
buildWarning list =
    Unknown (unwords ("I":list))

buildLogMessage :: [String] -> LogMessage
buildLogMessage (msgType : tokens)
    | msgType == "E" = buildError tokens
    | msgType == "I" = buildInfo tokens
    | msgType == "W" = buildWarning tokens
    | otherwise = Unknown (unwords (msgType : tokens))
buildLogMessage [] = Unknown ""

parseMessage :: String -> LogMessage
parseMessage = buildLogMessage . words

parseLog :: String -> [LogMessage]
parseLog str = map parseMessage (lines str)

getLogTimeStamp :: LogMessage -> Int
getLogTimeStamp (LogMessage _ timeStamp _) = timeStamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage (Node left nodeLogMessage right) =
    if getLogTimeStamp logMessage < getLogTimeStamp nodeLogMessage then
        Node (insert logMessage left) nodeLogMessage right
    else
        Node left nodeLogMessage (insert logMessage right)
insert logMessage Leaf =
    Node Leaf logMessage Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left mesage right) = 
    inOrder left ++ [mesage] ++ inOrder right