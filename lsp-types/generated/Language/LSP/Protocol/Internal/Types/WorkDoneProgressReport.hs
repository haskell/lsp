{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkDoneProgressReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons

{-|

-}
data WorkDoneProgressReport = WorkDoneProgressReport 
  { {-|

  -}
  kind :: (Language.LSP.Protocol.Types.Singletons.AString "report")
  , {-|
  Controls enablement state of a cancel button.

  Clients that don't support cancellation or don't support controlling the button's
  enablement state are allowed to ignore the property.
  -}
  cancellable :: (Maybe Bool)
  , {-|
  Optional, more detailed associated progress message. Contains
  complementary information to the `title`.

  Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
  If unset, the previous progress message (if any) is still valid.
  -}
  message :: (Maybe Data.Text.Text)
  , {-|
  Optional progress percentage to display (value 100 is considered 100%).
  If not provided infinite progress is assumed and clients are allowed
  to ignore the `percentage` value in subsequent in report notifications.

  The value should be steadily rising. Clients are free to ignore values
  that are not following this rule. The value range is [0, 100]
  -}
  percentage :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkDoneProgressReport)

instance Aeson.ToJSON WorkDoneProgressReport where
  toJSON (WorkDoneProgressReport arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,"cancellable" Language.LSP.Protocol.Types.Common..=? arg1
    ,"message" Language.LSP.Protocol.Types.Common..=? arg2
    ,"percentage" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON WorkDoneProgressReport where
  parseJSON = Aeson.withObject "WorkDoneProgressReport" $ \arg -> WorkDoneProgressReport <$> arg Aeson..: "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "cancellable" <*> arg Language.LSP.Protocol.Types.Common..:!? "message" <*> arg Language.LSP.Protocol.Types.Common..:!? "percentage"
