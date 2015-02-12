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

module Fragment ( Fragment
                , parseStdin
                , parseFile) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import System.IO (hGetContents, stdin)
import Text.Parsec
import Text.Parsec.String

data Fragment = Documentation String
              | BlockCode String [CodeOrReference]
              deriving (Eq, Show, Data, Typeable, Generic)

data CodeOrReference = Code String
                     | Reference String
                     deriving (Eq, Show, Data, Typeable, Generic)

parseStdin :: IO (Either ParseError [Fragment])
parseStdin = parse literateFile "<stdin>" <$> hGetContents stdin

parseFile :: FilePath -> IO (Either ParseError [Fragment])
parseFile = parseFromFile literateFile

literateFile :: Parser [Fragment]
literateFile = many (blockCode <|> documentation)

documentation :: Parser Fragment
documentation = do
  body <- many1Till anyChar (eof <|> lookAhead (void blockCode))
  return $ Documentation body

blockCode :: Parser Fragment
blockCode = do
  void $ string "<<"
  name <- many1Till anyChar (try (string ">>=" <?> "start of code block"))
  body <- many1Till (reference <|> code) (char '@')
  return $ BlockCode name body

code :: Parser CodeOrReference
code = do
  body <- many1Till anyChar (lookAhead $ (void (char '@'))
                                         <|> (void reference))
  return $ Code body

reference :: Parser CodeOrReference
reference = do
  void $ string "<<"
  name <- many1Till anyChar (try $ string ">>")
  return $ Reference name

many1Till :: Stream s m t
             => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = (:) <$> p <*> manyTill p end
