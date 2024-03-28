{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FormattingOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Value-object describing what options formatting should use.
-}
data FormattingOptions = FormattingOptions 
  { {-|
  Size of a tab in spaces.
  -}
  tabSize :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  Prefer spaces over tabs.
  -}
  insertSpaces :: Bool
  , {-|
  Trim trailing whitespace on a line.

  @since 3.15.0
  -}
  trimTrailingWhitespace :: (Maybe Bool)
  , {-|
  Insert a newline character at the end of the file if one does not exist.

  @since 3.15.0
  -}
  insertFinalNewline :: (Maybe Bool)
  , {-|
  Trim all newlines after the final newline at the end of the file.

  @since 3.15.0
  -}
  trimFinalNewlines :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FormattingOptions)

instance Aeson.ToJSON FormattingOptions where
  toJSON (FormattingOptions arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  [["tabSize" Aeson..= arg0]
    ,["insertSpaces" Aeson..= arg1]
    ,"trimTrailingWhitespace" Language.LSP.Protocol.Types.Common..=? arg2
    ,"insertFinalNewline" Language.LSP.Protocol.Types.Common..=? arg3
    ,"trimFinalNewlines" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON FormattingOptions where
  parseJSON = Aeson.withObject "FormattingOptions" $ \arg -> FormattingOptions <$> arg Aeson..: "tabSize" <*> arg Aeson..: "insertSpaces" <*> arg Language.LSP.Protocol.Types.Common..:!? "trimTrailingWhitespace" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertFinalNewline" <*> arg Language.LSP.Protocol.Types.Common..:!? "trimFinalNewlines"
