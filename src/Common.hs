module Common where

import qualified Data.Map as M
import           Data.Map (lookup)
import           Data.Tuple (swap)


lookupReverse :: Ord v => v -> M.Map k v -> Maybe k
lookupReverse v m = M.lookup v $ flipMap m

-- lookupReverseKV :: v -> M.Map k v -> Maybe (k, v)
-- lookupReverseKV = ...

flipMap :: Ord v => M.Map k v -> M.Map v k
flipMap m = M.fromList $ map swap (M.toList m)

-- TODO see also Data.List.Safe
findElemIndex :: [a] -> Int -> Maybe a
findElemIndex []     _ = Nothing
findElemIndex (x:_)  0 = Just x
findElemIndex (_:xs) i = xs !? (i-1)

-- TODO see also Data.List.Safe
(!?) = findElemIndex

(|>) = flip ($)
