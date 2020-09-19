module Cleaner where

-- starting with white space
-- starting with * means comment
-- // comment
-- /** */
isCommentOpen x = x `elem` ["//","/**"] 

cleanLine :: String -> String
cleanLine = unwords . takeWhile (not . isCommentOpen) . words 

removeLine :: String -> Bool
removeLine "" = False
removeLine ('*':xs) = False
removeLine _ = True

