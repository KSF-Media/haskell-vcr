module Test.Tasty.Ingredients.Vcr where

import qualified Data.Char              as Char
import           Data.Proxy             (Proxy (Proxy))
import qualified Data.Vcr               as Vcr
import qualified Options.Applicative    as Options
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.Ingredients as Tasty
import qualified Test.Tasty.Options     as Tasty

tastyIngredient :: Tasty.Ingredient
tastyIngredient =
  Tasty.includingOptions
    [ Tasty.Option (Proxy :: Proxy EnabledFlag)
    , Tasty.Option (Proxy :: Proxy RecordModeOption)
    ]

newtype RecordModeOption = RecordModeOption Vcr.RecordMode
  deriving (Show, Eq, Ord)

instance Tasty.IsOption RecordModeOption where
  defaultValue = RecordModeOption Vcr.Always
  parseValue v =
    RecordModeOption <$> case map Char.toLower v of
      "always" -> Just Vcr.Always
      _        -> Nothing
  optionName = pure "vcr-record-mode"
  optionHelp = pure "A mode of recording HTTP interactions (default: 'always')"
  optionCLParser = Tasty.mkOptionCLParser $ Options.metavar "always"

data EnabledFlag = EnabledFlag Bool
  deriving (Show, Eq, Ord)

instance Tasty.IsOption EnabledFlag where
  defaultValue = EnabledFlag False
  parseValue =
    fmap EnabledFlag . Tasty.safeReadBool
  optionName = pure "vcr"
  optionHelp = pure "Enable VCR (recording/replaying of HTTP interactions)"
  optionCLParser = Tasty.flagCLParser Nothing $ EnabledFlag True
