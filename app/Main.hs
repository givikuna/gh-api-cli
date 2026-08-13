{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.Environment (getArgs)

import CLI.Parser (Command (..), parseArgs)

main :: IO ()
main = do
    args :: [String] <- getArgs
    let command :: Command = parseArgs args
    print command
