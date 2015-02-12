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

module Fragment where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (void)
import System.IO (hGetContents, stdin)
import Text.Parsec
import Text.Parsec.String

data Fragment = Documentation String
              | BlockCode String String
              deriving (Eq, Show, Data, Typeable, Generic)

literateFile :: Parser [Fragment]
literateFile = many (blockCode <|> documentation)

documentation :: Parser Fragment
documentation = do
  body <- many1Till anyChar (eof <|> lookAhead (void blockCode))
  return $ Documentation body

blockCode :: Parser Fragment
blockCode = do
  void $ string "<<"
  name <- manyTill anyChar (try $ string ">>=")
  body <- manyTill anyChar (char '@')
  return $ BlockCode name body

many1Till :: Stream s m t
             => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = (:) <$> p <*> manyTill p end
