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

module Tangle (tangle) where

import Control.Exception (assert)
import Control.Monad (liftM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Fragment (Fragment, isBlockCode, blockName, blockContents,
                 CodeOrReference(Code, Reference))

type FragmentGraph = Map String [CodeOrReference]

tangle :: [Fragment] -> Either String String
tangle fragments =
  case filter isBlockCode fragments of
    [] -> Right ""
    codeBlocks@(root:_) ->
      expandBlock (fragmentGraph codeBlocks) (blockName root)

fragmentGraph :: [Fragment] -> FragmentGraph
fragmentGraph frags =
  Map.fromListWith (++) $
    map (\block -> blockToPair $ assert (isBlockCode block) block) frags
  where blockToPair frag = (blockName frag, blockContents frag)

expandBlock :: FragmentGraph -> String -> Either String String
expandBlock fragments name =
  case Map.lookup name fragments of
    Nothing -> Left $ "Desired node " ++ name ++ " not in fragment graph"
    Just block -> concatMapM (expandBlockBody1 fragments) block

expandBlockBody1 :: FragmentGraph -> CodeOrReference -> Either String String
expandBlockBody1 _ (Code body) = Right body
expandBlockBody1 fragments (Reference name) = expandBlock fragments name

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f lists = liftM concat $ mapM f lists
