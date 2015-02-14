{- Copyright © 2015 Benjamin Barenblat

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>. -}

{-# LANGUAGE RecordWildCards #-}
module Tangle (tangle) where

import Control.Exception (assert)
import Control.Monad (liftM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Fragment (Fragment(..), CodeOrReference(..))

type FragmentGraph = Map String [CodeOrReference]

tangle :: [Fragment] -> Either String String
tangle fragments =
  case filter isBlockCode fragments of
    [] -> Right ""
    blockCodeFragments@((BlockCode rootName _):_) ->
      expandBlockCodeFragment (fragmentGraph blockCodeFragments) rootName
    (Documentation {..}):_ -> error "isBlockCode did not work correctly"

fragmentGraph :: [Fragment] -> FragmentGraph
fragmentGraph =
  Map.fromListWith (flip (++)) .
    map (\block -> blockToPair $ assert (isBlockCode block) block)
  where blockToPair (BlockCode name body) = (name, body)
        blockToPair (Documentation {..}) =
          error "Documentation fragments cannot be converted to pairs"

expandBlockCodeFragment :: FragmentGraph -> String -> Either String String
expandBlockCodeFragment fragments name =
  case Map.lookup name fragments of
    Nothing -> Left $ "Desired node " ++ name ++ " not in fragment graph"
    Just block -> concatMapM (expandBlockCodeBody fragments) block

expandBlockCodeBody :: FragmentGraph -> CodeOrReference -> Either String String
expandBlockCodeBody _ (Code body) = Right body
expandBlockCodeBody fragments (Reference name) =
  expandBlockCodeFragment fragments name

isBlockCode :: Fragment -> Bool
isBlockCode (Documentation {..}) = False
isBlockCode (BlockCode {..}) = True

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f lists = liftM concat $ mapM f lists
