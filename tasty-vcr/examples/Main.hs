{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Monoid        ((<>))
import           Data.Proxy         (Proxy (..))
import           Test.Tasty         as Tasty
import           Test.Tasty.HUnit   as HUnit
import           Test.Tasty.Options as Tasty
import           Test.Tasty.Vcr     as Vcr

main :: IO ()
main = do
  Tasty.defaultMainWithIngredients ingredients tests
  where
    ingredients =
      Tasty.defaultIngredients
        <>
      [ Tasty.includingOptions
          [ Tasty.Option (Proxy :: Proxy Vcr.EnabledFlag)
          , Tasty.Option (Proxy :: Proxy Vcr.RecordModeOption)
          ]
      ]

tests :: Tasty.TestTree
tests =
  askOption $ \(Vcr.EnabledFlag vcrEnabled) ->
    askOption $ \(Vcr.RecordModeOption vcrMode) ->
      HUnit.testCase "Some test" $ do
        if vcrEnabled
          then putStrLn $ "VCR is " <> show vcrMode <> " rocking the boat"
          else putStrLn $ "No VCR for you, rock the boat yourself"


