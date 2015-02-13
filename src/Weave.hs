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

module Weave (weave) where

import Control.Monad (liftM)
import Data.Functor ((<$>))

import Fragment

weave :: [Fragment] -> Either String String
weave = concatMapM weaveFragment

weaveFragment :: Fragment -> Either String String
weaveFragment (Documentation text) = Right text
weaveFragment (BlockCode name body) = do
  escapeCharacter <- texEscape <$> pickTexEscapeCharacter body
  return $ "\\begin{LytBlockCode}{" ++ name ++ "}{" ++ escapeCharacter ++ "}\n"
           ++ weaveBlockBody escapeCharacter body
           ++ "\\end{LytBlockCode}\n"

weaveBlockBody :: String -> [CodeOrReference] -> String
weaveBlockBody escapeCharacter ((Code first):rest) =
  {- The first block is code.  To make sure it gets typeset correctly, drop
  everything up to the first newline or non-space character. -}
  (case dropWhile (==' ') first of
     '\n' : first' -> first'
     first' -> first')
  ++ concatMap (weaveCodeOrReference escapeCharacter) rest
weaveBlockBody escapeCharacter blocks =
  concatMap (weaveCodeOrReference escapeCharacter) blocks

weaveCodeOrReference :: String -> CodeOrReference -> String
weaveCodeOrReference _ (Code text) = text
weaveCodeOrReference escapeCharacter (Reference name) =
  escapeCharacter ++ "\\LytFragmentReference{" ++ name ++ "}" ++ escapeCharacter

pickTexEscapeCharacter :: [CodeOrReference] -> Either String Char
pickTexEscapeCharacter blocks =
  case firstNotIn (consolidate blocks) texPotentialEscapeCharacters of
    Just c -> Right c
    Nothing -> Left "could not find a suitable LaTeX escape character"
  where consolidate = foldl (\current block -> case block of
                                Code text -> current ++ text
                                Reference _ -> current)
                            ""

firstNotIn :: Eq a => [a] -> [a] -> Maybe a
firstNotIn _ [] = Nothing
firstNotIn haystack (x:xs)
  | x `elem` haystack = firstNotIn haystack xs
  | otherwise = Just x

texPotentialEscapeCharacters :: [Char]
texPotentialEscapeCharacters =
  "!@#$%^&*()-_+={}[]|\"':;/?,.~`" ++ ['A' .. 'Z' ] ++ ['a' .. 'z']

texEscape :: Char -> String
texEscape char
  | char `elem` "#$%^&_{}~" = ['\\', char]
  | otherwise = [char]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f lists = liftM concat $ mapM f lists
