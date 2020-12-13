module Lib.List
  ( groupedIn
  ) where

groupedIn :: Int -> [a] -> [[a]]
groupedIn size = groupedIn' []
 where
  groupedIn' acc [] = acc
  groupedIn' acc as = groupedIn' (acc <> [take size as]) (drop size as)
