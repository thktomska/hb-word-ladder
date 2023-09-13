module Data.AssocMap
  ( AssocMap(..)
  , empty
  , insert
  , delete
  , alter
  , member
  , lookup
  , findWithDefault
  )
where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

newtype AssocMap k v = AssocMap [(k, v)]
    deriving (Show)

empty :: AssocMap k v
empty = AssocMap []

insert :: Eq k => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

delete :: Eq k => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter fn k (AssocMap as) = AssocMap (alter' fn k as)
  where
    alter' :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
    alter' f key [] =
      case f Nothing of
        Nothing -> []
        Just value -> [(key, value)]
    alter' f key ((key', value') : xs)
      | key == key' =
        case f (Just value') of
          Nothing -> xs
          Just value -> (key, value) : xs
      | otherwise =
        (key', value') : alter' f key xs

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap as) = member' key as
  where
    member' :: Eq k => k -> [(k, v)] -> Bool
    member' _ [] = False
    member' x ((x', _) : xs)
        | x' == x = True
        | otherwise = member' x xs

-- hasNode :: Eq k => AssocMap k v -> k -> Bool
-- hasNode = flip member

-- addNode :: Eq k => AssocMap k v -> k -> AssocMap k v
-- addNode ascMap node
--     | ascMap `hasNode` node = ascMap
--     | otherwise = (node, []) : ascMap

-- myAssocList :: [(Int, Int)]
-- myAssocList = [(1,1), (2,2), (3,3)] :: [(Int, Int)]

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup k (AssocMap as) = lookup' k as
  where
    lookup' _ [] = Nothing
    lookup' key ((key', value) : xs)
      | key == key' = Just value
      | otherwise = lookup' key xs

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key ascMap = fromMaybe defaultValue (lookup key ascMap)
