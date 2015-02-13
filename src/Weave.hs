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

import Fragment

weave :: [Fragment] -> Either String String
weave = Right . concatMap weaveFragment

weaveFragment :: Fragment -> String
weaveFragment (Documentation text) = text
weaveFragment (BlockCode name body) =
  "\\begin{LytBlockCode}{" ++ name ++ "}\n"
  ++ weaveBlockBody body
  ++ "\\end{LytBlockCode}\n"

weaveBlockBody :: [CodeOrReference] -> String
weaveBlockBody ((Code first):rest) =
  {- The first block is code.  To make sure it gets typeset correctly, drop
  everything up to the first newline or non-space character. -}
  (case dropWhile (==' ') first of
     '\n' : first' -> first'
     first' -> first')
  ++ concatMap weaveCodeOrReference rest
weaveBlockBody blocks = concatMap weaveCodeOrReference blocks

weaveCodeOrReference :: CodeOrReference -> String
weaveCodeOrReference (Code text) = text
weaveCodeOrReference (Reference name) = "\\LytFragmentReference{" ++ name ++ "}"
