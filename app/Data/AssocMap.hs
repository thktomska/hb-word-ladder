module Data.AssocMap
  ( AssocMap(..)
  , member
  , alter
  )
where

newtype AssocMap k v = AssocMap [(k, v)]
    deriving (Show)

empty :: AssocMap k v
empty = AssocMap []

insert :: Eq k => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

delete :: Eq k => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

-- alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
-- alter f key (AssocMap []) = maybe (AssocMap []) (\value -> AssocMap [(key, value)]) (f Nothing)
-- alter f key ((key', value') : xs)
--     | key == key' =
--         maybe xs (\value -> (key, value) : xs) (f Nothing)
--     | otherwise =
--         (key', value') : alter f key xs


alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap xs) = AssocMap (alter' f key xs)
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
member k (AssocMap xs) = case lookup k xs of
    Nothing -> False
    _       -> True

-- hasNode :: Eq k => AssocMap k v -> k -> Bool
-- hasNode = flip member

-- addNode :: Eq k => AssocMap k v -> k -> AssocMap k v
-- addNode ascMap node
--     | ascMap `hasNode` node = ascMap
--     | otherwise = (node, []) : ascMap

-- myAssocList :: [(Int, Int)]
-- myAssocList = [(1,1), (2,2), (3,3)] :: [(Int, Int)]
