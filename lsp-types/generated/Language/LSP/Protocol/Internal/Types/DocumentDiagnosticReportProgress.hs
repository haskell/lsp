{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportProgress where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportPartialResult
import qualified Language.LSP.Protocol.Types.Common

{-|
The document diagnostic report used when reporting partial result.

When using partial results, the first literal sent needs to be a
DocumentDiagnosticReport providing the diagnostics on the document
followed by n DocumentDiagnosticReportPartialResult literals providing
the diagnostics for related documents.

```
DocumentDiagnosticReport
DocumentDiagnosticReportPartialResult
DocumentDiagnosticReportPartialResult
...
```

@since 3.18.1
-}
newtype DocumentDiagnosticReportProgress = DocumentDiagnosticReportProgress (Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReport.DocumentDiagnosticReport Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportPartialResult.DocumentDiagnosticReportPartialResult)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentDiagnosticReportProgress)
