{-# LANGUAGE OverloadedStrings #-}

import CMarkGFM
import Test.HUnit
import System.Exit
import Data.Text ()

main :: IO ()
main = do
  counts' <- runTestTT tests
  case (errors counts' + failures counts') of
       0 -> exitWith ExitSuccess
       n -> exitWith (ExitFailure n)

-- The C library has its own extensive tests.
-- Here we just make sure it's basically working.
tests :: Test
tests = TestList [
    "<h1>Hi</h1>\n" ~=? commonmarkToHtml [] [] "# Hi"
  , "<p>dog’s</p>\n" ~=? commonmarkToHtml [optSmart] [] "dog's"
  , "<p><a href=\"\">trick</a></p>\n" ~=? commonmarkToHtml [optSafe] [] "[trick](javascript:alert('hi'))"
  , ".RS\n.PP\nquote\n.RE\n" ~=? commonmarkToMan [] [] Nothing "> quote"
  , (Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 13})) DOCUMENT [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 13})) PARAGRAPH [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 6})) (TEXT "Hello ") [],Node (Just (PosInfo {startLine = 1, startColumn = 7, endLine = 1, endColumn = 13})) EMPH [Node (Just (PosInfo {startLine = 1, startColumn = 8, endLine = 1, endColumn = 12})) (TEXT "world") []]]]) ~=? commonmarkToNode [] [] "Hello *world*"
  , "> Hello\n> *world*\n" ~=? nodeToCommonmark [] (Just 12) (Node Nothing DOCUMENT [Node Nothing BLOCK_QUOTE [Node Nothing PARAGRAPH [Node Nothing (TEXT "Hello ") [],Node Nothing EMPH [Node Nothing (TEXT "world") []]]]])
  , "<p>~hi~</p>\n" ~=? commonmarkToHtml [] [] "~hi~"
  , "<p><del>hi</del></p>\n" ~=? commonmarkToHtml [] [extStrikethrough] "~hi~"
  , (Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 4})) DOCUMENT [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 4})) PARAGRAPH [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 4})) STRIKETHROUGH [Node (Just (PosInfo {startLine = 1, startColumn = 2, endLine = 1, endColumn = 3})) (TEXT "hi") []]]]) ~=? commonmarkToNode [] [extStrikethrough] "~hi~"
  , "<p>www.google.com</p>\n" ~=? commonmarkToHtml [] [] "www.google.com"
  , "<p><a href=\"http://www.google.com\">www.google.com</a></p>\n" ~=? commonmarkToHtml [] [extAutolink] "www.google.com"
  , "<p>| a |\n| --- |\n| b |</p>\n" ~=? commonmarkToHtml [] [] "| a |\n| --- |\n| b |\n"
  , "<table>\n<thead>\n<tr>\n<th>a</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>b</td>\n</tr></tbody></table>\n" ~=? commonmarkToHtml [] [extTable] "| a |\n| --- |\n| b |\n"
  , Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 3, endColumn = 17})) DOCUMENT [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 3, endColumn = 17})) (TABLE [LeftAligned,CenterAligned,NoAlignment,RightAligned]) [Node (Just (PosInfo {startLine = 1, startColumn = 1, endLine = 1, endColumn = 17})) TABLE_ROW [Node (Just (PosInfo {startLine = 1, startColumn = 3, endLine = 1, endColumn = 4})) TABLE_CELL [Node (Just (PosInfo {startLine = 1, startColumn = 3, endLine = 1, endColumn = 3})) (TEXT "a") []],Node (Just (PosInfo {startLine = 1, startColumn = 7, endLine = 1, endColumn = 8})) TABLE_CELL [Node (Just (PosInfo {startLine = 1, startColumn = 7, endLine = 1, endColumn = 7})) (TEXT "b") []],Node (Just (PosInfo {startLine = 1, startColumn = 11, endLine = 1, endColumn = 12})) TABLE_CELL [Node (Just (PosInfo {startLine = 1, startColumn = 11, endLine = 1, endColumn = 11})) (TEXT "c") []],Node (Just (PosInfo {startLine = 1, startColumn = 15, endLine = 1, endColumn = 16})) TABLE_CELL [Node (Just (PosInfo {startLine = 1, startColumn = 15, endLine = 1, endColumn = 15})) (TEXT "d") []]],Node (Just (PosInfo {startLine = 3, startColumn = 1, endLine = 3, endColumn = 17})) TABLE_ROW [Node (Just (PosInfo {startLine = 3, startColumn = 3, endLine = 3, endColumn = 4})) TABLE_CELL [Node (Just (PosInfo {startLine = 3, startColumn = 3, endLine = 3, endColumn = 3})) (TEXT "y") []],Node (Just (PosInfo {startLine = 3, startColumn = 7, endLine = 3, endColumn = 8})) TABLE_CELL [Node (Just (PosInfo {startLine = 3, startColumn = 7, endLine = 3, endColumn = 7})) (TEXT "o") []],Node (Just (PosInfo {startLine = 3, startColumn = 11, endLine = 3, endColumn = 12})) TABLE_CELL [Node (Just (PosInfo {startLine = 3, startColumn = 11, endLine = 3, endColumn = 11})) (TEXT "s") []],Node (Just (PosInfo {startLine = 3, startColumn = 15, endLine = 3, endColumn = 16})) TABLE_CELL [Node (Just (PosInfo {startLine = 3, startColumn = 15, endLine = 3, endColumn = 15})) (TEXT "h") []]]]] ~=? commonmarkToNode [] [extTable] "| a | b | c | d |\n| :-- | :-: | --- | --: |\n| y | o | s | h |"
  , "<xmp>\n" ~=? commonmarkToHtml [] [] "<xmp>"
  , "&lt;xmp>\n" ~=? commonmarkToHtml [] [extTagfilter] "<xmp>"
  ]
