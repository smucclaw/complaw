{- |
Copyright: (c) 2020 Wong Meng Weng
SPDX-License-Identifier: MIT
Maintainer: Wong Meng Weng <mwwong@smu.edu.sg>

Hello World for a CCLAW RaC project
-}

module Helloworld
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
