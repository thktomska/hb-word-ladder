module Ladder
  ( readDictionary
  , mkLadderGraph
  , computeCandidates
  )
where

import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM

type Dictionary = [String]

readDictionary :: FilePath -> IO Dictionary
readDictionary filepath = do
    dictionaryContent <- readFile filepath
    let
        lines' = L.lines dictionaryContent
        words' = L.map (L.filter (`L.elem` ['a' .. 'z'])) lines'
    return (L.nub words')

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph nodes
  where
    map' = PM.createPermutationMap dict
    nodes =
      L.map (\w -> (w, computeCandidates map' w)) dict

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates map' word =
  let candidates = modified ++ removed ++ added ++ [word]
      uniques = L.nub . L.sort $ candidates
      perms = L.concatMap (\x -> PM.findWithDefault [] x map') uniques
   in L.delete word perms
  where
    added = [x : word | x <- ['a' .. 'z']]
    removed = [L.delete x word | x <- word]
    modified =
      [x : L.delete y word | x <- ['a' .. 'z'], y <- word, x /= y]
