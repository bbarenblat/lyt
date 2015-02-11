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
                , parseFile
                , ParseError) where

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

parseStdin :: IO (Either ParseError [Fragment])
parseStdin = parse literateFile "<stdin>" <$> hGetContents stdin

parseFile :: FilePath -> IO (Either ParseError [Fragment])
parseFile = parseFromFile literateFile

literateFile :: Parser [Fragment]
literateFile = alternate documentation blockCode

documentation :: Parser Fragment
documentation = Documentation <$> manyTill anyChar (void (string "<<") <|> eof)

blockCode :: Parser Fragment
blockCode =
  BlockCode <$> manyTill anyChar (void $ string ">>=")
            <*> manyTill anyChar (void $ char '@')

alternate :: Parser a -> Parser a -> Parser [a]
alternate x y = (:) <$> x <*> (alternate y x <|> pure [])
