module Hw2 where

import Data.List (intercalate)

data MessageType
  = Info
  | Warning
  | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage
  = LogMessage MessageType TimeStamp String
  | Unknown String
  deriving (Show, Eq)

data MessageTree
  = Leaf
  | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse ::
  (String -> [LogMessage]) ->
  Int ->
  FilePath ->
  IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong ::
  (String -> [LogMessage]) ->
  ([LogMessage] -> [String]) ->
  FilePath ->
  IO [String]
testWhatWentWrong parse whatWentWrong file =
  whatWentWrong . parse <$> readFile file

parseTimeAndMessage :: String -> (Integer, String)
parseTimeAndMessage s = case words s of
  (time : string) -> (read time, intercalate "" string)
  _ -> (3, "abc")

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : time : string) -> LogMessage Info (read time) (unwords string)
  ("W" : time : string) -> LogMessage Warning (read time) (unwords string)
  ("E" : severity : time : string) -> LogMessage (Error (read severity)) (read time) (unwords string)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message@(LogMessage _ time _) tree =
  case tree of
    Leaf -> Node Leaf message Leaf
    Node treeLeft message2@(LogMessage _ time2 _) treeRight ->
      if time < time2
        then Node (insert message treeLeft) message2 treeRight
        else Node treeLeft message2 (insert message treeRight)
    -- this case should be impossible
    Node _ (Unknown _) _ -> Leaf

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ (message : inOrder right)

filterMessage :: LogMessage -> Bool
filterMessage (LogMessage messageType _ _) = case messageType of
  Error severity -> severity > 50
  _ -> False
filterMessage _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map (\(LogMessage _ _ text) -> text) (inOrder (build (filter filterMessage list)))
