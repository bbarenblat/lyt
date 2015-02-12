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

{- Grammar:

S         → ε | TEXT | TEXT block S | block S
block     → << TEXT >>= code
code      → @ | TEXT @ | TEXT reference code | reference code
reference → << TEXT >>

TEXT -> any sequence of one or more Unicode code points -}

module Fragment ( Fragment(..)
                , CodeOrReference(..)
                , parseFragments) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

import Debug.Trace

data Fragment = Documentation String
              | BlockCode String [CodeOrReference]
              deriving (Eq, Show, Data, Typeable, Generic)

data CodeOrReference = Code String
                     | Reference String
                     deriving (Eq, Show, Data, Typeable, Generic)

parseFragments :: FilePath -> String -> Either String [Fragment]
parseFragments path input =
  case parse literateFile path input of
    Right result -> traceShow result $ Right result
    Left err -> Left $ show err

literateFile :: Parser [Fragment]
literateFile = (:) <$> blockCode <*> literateFile
               <|> do body <- try $ manyTill anyChar (lookAhead blockCode)
                      block <- blockCode
                      rest <- literateFile
                      return $ Documentation body : block : rest
               <|> (:[]) . Documentation <$> manyTill anyChar eof

blockCode :: Parser Fragment
blockCode = do
  void $ try $ string "<<"
  name <- many1Till (noneOf "\r\n") (try $ string ">>=")
  body <- code
  return $ BlockCode name body


data CodeTerminator = AtSign
                    | BeginReference
                    deriving (Eq, Show, Data, Typeable, Generic)

atSign :: Parser CodeTerminator
atSign = char '@' >> return AtSign

beginReference :: Parser CodeTerminator
beginReference = lookAhead reference >> return BeginReference

code :: Parser [CodeOrReference]
code = (:) <$> reference <*> code
       <|> do (body, exitChar) <- manyTill' anyChar (atSign <|> beginReference)
              case exitChar of
                AtSign -> return [Code body]
                BeginReference -> do
                  ref <- option [] $ (:[]) <$> reference
                  rest <- code
                  return $ Code body : ref ++ rest

reference :: Parser CodeOrReference
reference = do
  void $ try $ string "<<"
  name <- many1Till anyChar (try $ string ">>")
  return $ Reference name

manyTill' :: Stream s m t
             => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill' p end = scan
  where scan  = do exit <- end
                   return ([], exit)
                <|>
                do x <- p
                   (xs, exit) <- scan
                   return (x:xs, exit)

many1Till :: Stream s m t
             => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = (:) <$> p <*> manyTill p end
