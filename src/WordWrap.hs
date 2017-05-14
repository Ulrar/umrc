-- Taken from http://moreindirection.blogspot.ie/2010/08/blog-post.html

module WordWrap (wrapLine) where

import Data.Char (isSpace)

trim :: String -> String
trim = trimAndReverse . trimAndReverse
  where trimAndReverse = reverse . dropWhile isSpace

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

wrapLine :: Int -> String -> [String]
wrapLine maxLen line 
  | length line <= maxLen  = [line]
  | any isSpace beforeMax  = beforeSpace : (wrapLine maxLen $ afterSpace ++ afterMax)
  | otherwise              = beforeMax : wrapLine maxLen afterMax
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax
