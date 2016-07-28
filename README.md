# Control.Logger.Simple

A small and fast opinionated logging framework. Needs GHC >=7.10

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Logger.Simple

main :: IO ()
main =
    withGlobalLogging (LogConfig (Just "logfile.txt") True) $
    do logInfo "Hi!"
```
