{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestOptions
import qualified Language.LSP.Protocol.Internal.Types.TokenFormat
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensClientCapabilities = SemanticTokensClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  return value for the corresponding server capability as well.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  Which requests the client supports and might send to the server
  depending on the server's capability. Please note that clients might not
  show semantic tokens or degrade some of the user experience if a range
  or full request is advertised by the client but not provided by the
  server. If for example the client capability `requests.full` and
  `request.range` are both set to true but the server only provides a
  range provider the client might not render a minimap correctly or might
  even decide to not show any semantic tokens at all.
  -}
  requests :: Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestOptions.ClientSemanticTokensRequestOptions
  , {-|
  The token types that the client supports.
  -}
  tokenTypes :: [Data.Text.Text]
  , {-|
  The token modifiers that the client supports.
  -}
  tokenModifiers :: [Data.Text.Text]
  , {-|
  The token formats the clients supports.
  -}
  formats :: [Language.LSP.Protocol.Internal.Types.TokenFormat.TokenFormat]
  , {-|
  Whether the client supports tokens that can overlap each other.
  -}
  overlappingTokenSupport :: (Maybe Bool)
  , {-|
  Whether the client supports tokens that can span multiple lines.
  -}
  multilineTokenSupport :: (Maybe Bool)
  , {-|
  Whether the client allows the server to actively cancel a
  semantic token request, e.g. supports returning
  LSPErrorCodes.ServerCancelled. If a server does the client
  needs to retrigger the request.

  @since 3.17.0
  -}
  serverCancelSupport :: (Maybe Bool)
  , {-|
  Whether the client uses semantic tokens to augment existing
  syntax tokens. If set to `true` client side created syntax
  tokens and semantic tokens are both used for colorization. If
  set to `false` the client only uses the returned semantic tokens
  for colorization.

  If the value is `undefined` then the client behavior is not
  specified.

  @since 3.17.0
  -}
  augmentsSyntaxTokens :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensClientCapabilities)

instance Aeson.ToJSON SemanticTokensClientCapabilities where
  toJSON (SemanticTokensClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,["requests" Aeson..= arg1]
    ,["tokenTypes" Aeson..= arg2]
    ,["tokenModifiers" Aeson..= arg3]
    ,["formats" Aeson..= arg4]
    ,"overlappingTokenSupport" Language.LSP.Protocol.Types.Common..=? arg5
    ,"multilineTokenSupport" Language.LSP.Protocol.Types.Common..=? arg6
    ,"serverCancelSupport" Language.LSP.Protocol.Types.Common..=? arg7
    ,"augmentsSyntaxTokens" Language.LSP.Protocol.Types.Common..=? arg8]

instance Aeson.FromJSON SemanticTokensClientCapabilities where
  parseJSON = Aeson.withObject "SemanticTokensClientCapabilities" $ \arg -> SemanticTokensClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Aeson..: "requests" <*> arg Aeson..: "tokenTypes" <*> arg Aeson..: "tokenModifiers" <*> arg Aeson..: "formats" <*> arg Language.LSP.Protocol.Types.Common..:!? "overlappingTokenSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "multilineTokenSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "serverCancelSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "augmentsSyntaxTokens"
