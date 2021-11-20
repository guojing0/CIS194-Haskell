{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage xs = case firstElt of
                    "I" -> LogMessage Info secondElt (unwords restMsg)
                    "W" -> LogMessage Warning secondElt (unwords restMsg)
                    "E" -> LogMessage (Error secondElt) (read (head restMsg) :: Int) (unwords $ drop 1 restMsg)
                    _   -> Unknown xs
    where wordedList = words xs
          firstElt   = head wordedList
          secondElt  = read (wordedList !! 1) :: Int
          restMsg    = drop 2 wordedList

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ timeStamp _) (Node leftTree treeM@(LogMessage _ treeTimeStamp _) rightTree)
    | timeStamp <= treeTimeStamp = Node (insert m leftTree) treeM rightTree
    | otherwise                  = Node leftTree treeM (insert m rightTree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [ msg | (LogMessage (Error num) _ msg) <- inOrder (build xs), num > 50]
