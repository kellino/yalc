{-# LANGUAGE OverloadedStrings #-}

module Main where

import Unbound.Generics.LocallyNameless
import Text.Megaparsec
import Data.Either (partitionEithers)
import Data.Text

import Syntax
import Parser
import Env
import TypeCheck

go :: Text -> IO ()
go t = 
    case parseProgram t of
      Left badparse -> putStrLn $ errorBundlePretty badparse
      Right parsed -> do
          let (bad, [s,d]) = partitionEithers parsed
          let res = runTypeCheck s d emptyCtx
          case res of
            Left err -> print err
            Right trm -> print trm

main :: IO ()
main = go "id : Bool -> Bool\nid x = x"
