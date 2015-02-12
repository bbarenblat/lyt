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

{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Data.Functor ((<$>))
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Fragment (parseFragments)
import Tangle (tangle)
import Weave (weave)

data Operation = Tangle | Weave
                  deriving (Eq, Show, Data, Typeable, Generic, Read)

parseOp :: String -> Maybe Operation
parseOp "tangle" = Just Tangle
parseOp "weave" = Just Weave
parseOp _ = Nothing

data Configuration = Configuration Operation (Maybe FilePath)
                   deriving (Eq, Show, Data, Typeable, Generic)

main :: IO ()
main = do
  processArguments <$> getArgs >>= \case
    Nothing -> usage >> exitFailure
    Just (Configuration op file) -> do
      let pathString = case file of Nothing -> "<stdin>"
                                    Just path -> path
      input <- case file of Nothing -> getContents
                            Just path -> readFile path
      case doOperation op (pathString, input) of
        Left err -> putStrLn err >> exitFailure
        Right result -> putStr result

doOperation :: Operation -> (FilePath, String) -> Either String String
doOperation op (path, input) = do
  parsed <- parseFragments path input
  case op of
    Tangle -> tangle parsed
    Weave -> weave parsed

processArguments :: [String] -> Maybe Configuration
processArguments [opString] =
  case parseOp opString of
    Just op -> Just $ Configuration op Nothing
    Nothing -> Nothing
processArguments [opString, file] =
  case parseOp opString of
    Just op -> Just $ Configuration op (Just file)
    Nothing -> Nothing
processArguments _ = Nothing

usage :: IO ()
usage = putStrLn "usage: lyt (tangle|weave) [file]"
