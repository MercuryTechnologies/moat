{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Moat
  ( mobileGenWith
  , prettySwiftData
  , toMoatData
  , Options (..)
  , defaultOptions
  , prettyKotlinData
  , Annotation (..)
  , Interface (..)
  , Protocol (..)
  )
import Data.Proxy (Proxy (..))

data User =
  User 
    { firstName :: String
    , lastName :: String
    , age :: Int
    }

$(mobileGenWith
  defaultOptions
    { dataProtocols = [Codable]
    , dataInterfaces = [Parcelable]
    , dataAnnotations = [Parcelize]
    }
  ''User
 )

generateSwiftCode :: String
generateSwiftCode = prettySwiftData . toMoatData $ Proxy @User

generateKotlinCode :: String
generateKotlinCode = prettyKotlinData . toMoatData $ Proxy @User

main :: IO ()
main = do
  putStrLn "Swift Type"
  putStrLn generateSwiftCode
  putStrLn ""
  putStrLn "Kotlin Type"
  putStrLn generateKotlinCode
