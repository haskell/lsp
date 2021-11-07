{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Signature Help Request
-- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#signature-help-request
module Language.LSP.Types.SignatureHelp where
    
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Language.LSP.Types.Common
import Language.LSP.Types.MarkupContent
import Language.LSP.Types.Progress
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils
import Control.Applicative (Alternative((<|>)))

-- -------------------------------------

data SignatureHelpParameterInformation =
  SignatureHelpParameterInformation
    { -- | The client supports processing label offsets instead of a simple
      -- label string.
      --
      -- @since 3.14.0
      _labelOffsetSupport :: Maybe Bool
    }
  deriving (Read, Show, Eq)
deriveJSON lspOptions ''SignatureHelpParameterInformation

data SignatureHelpSignatureInformation =
  SignatureHelpSignatureInformation
    { -- | Client supports the follow content formats for the documentation
      -- property. The order describes the preferred format of the client.
      _documentationFormat :: Maybe (List MarkupKind)
      -- | Client capabilities specific to parameter information.
    , _parameterInformation :: Maybe SignatureHelpParameterInformation
      -- | The client supports the `activeParameter` property on
      -- 'SignatureInformation' literal.
      --
      -- @since 3.16.0
    , _activeParameterSuport :: Maybe Bool
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''SignatureHelpSignatureInformation

data SignatureHelpClientCapabilities =
  SignatureHelpClientCapabilities
    { -- | Whether signature help supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
      -- | The client supports the following 'SignatureInformation'
      -- specific properties.
    , _signatureInformation :: Maybe SignatureHelpSignatureInformation
      -- | The client supports to send additional context information for a
      -- @textDocument/signatureHelp@ request. A client that opts into
      -- contextSupport will also support the '_retriggerCharacters' on
      -- 'SignatureHelpOptions'.
      --
      -- @since 3.15.0
    , _contextSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''SignatureHelpClientCapabilities

-- -------------------------------------

makeExtendingDatatype "SignatureHelpOptions" [''WorkDoneProgressOptions]
  [ ("_triggerCharacters", [t| Maybe (List Text) |])
  , ("_retriggerCharacters", [t| Maybe (List Text) |])
  ]
deriveJSON lspOptions ''SignatureHelpOptions

makeExtendingDatatype "SignatureHelpRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''SignatureHelpOptions
  ] []
deriveJSON lspOptions ''SignatureHelpRegistrationOptions

-- -------------------------------------

data SignatureHelpDoc = SignatureHelpDocString Text | SignatureHelpDocMarkup MarkupContent
  deriving (Read,Show,Eq)

deriveJSON lspOptionsUntagged ''SignatureHelpDoc

-- -------------------------------------

data ParameterLabel = ParameterLabelString Text | ParameterLabelOffset Word32 Word32
  deriving (Read,Show,Eq)

instance ToJSON ParameterLabel where
  toJSON (ParameterLabelString t) = toJSON t
  toJSON (ParameterLabelOffset l h) = toJSON [l, h]

instance FromJSON ParameterLabel where
  parseJSON x = ParameterLabelString <$> parseJSON x <|> parseInterval x
    where
      parseInterval v@(Array _) = do
        is <- parseJSON v
        case is of
          [l, h] -> pure $ ParameterLabelOffset l h
          _ -> fail "ParameterLabel"
      parseInterval _ = fail "ParameterLabel"

-- -------------------------------------

{-| 
Represents a parameter of a callable-signature. A parameter can
have a label and a doc-comment.
-}
data ParameterInformation =
  ParameterInformation
    { _label         :: ParameterLabel -- ^ The label of this parameter information.
    , _documentation :: Maybe SignatureHelpDoc -- ^ The human-readable doc-comment of this parameter.
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''ParameterInformation

-- -------------------------------------

{-|
Represents the signature of something callable. A signature
can have a label, like a function-name, a doc-comment, and
a set of parameters.
-}
data SignatureInformation =
  SignatureInformation
    { _label           :: Text -- ^ The label of the signature.
    , _documentation   :: Maybe SignatureHelpDoc -- ^ The human-readable doc-comment of this signature.
    , _parameters      :: Maybe (List ParameterInformation) -- ^ The parameters of this signature.
    , _activeParameter :: Maybe Word32 -- ^ The index of the active parameter.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureInformation


{-|
Signature help represents the signature of something
callable. There can be multiple signature but only one
active and only one active parameter.
-}
data SignatureHelp =
  SignatureHelp
    { _signatures      :: List SignatureInformation -- ^ One or more signatures.
    , _activeSignature :: Maybe Word32 -- ^ The active signature.
    , _activeParameter :: Maybe Word32 -- ^ The active parameter of the active signature.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureHelp

-- -------------------------------------

-- | How a signature help was triggered.
--
-- @since 3.15.0
data SignatureHelpTriggerKind = SHTKInvoked -- ^ Signature help was invoked manually by the user or by a command.
                              | SHTKTriggerCharacter -- ^ Signature help was triggered by a trigger character.
                              | SHTKContentChange -- ^ Signature help was triggered by the cursor moving or by the document content changing.
  deriving (Read,Show,Eq)

instance ToJSON SignatureHelpTriggerKind where
  toJSON SHTKInvoked          = Number 1
  toJSON SHTKTriggerCharacter = Number 2
  toJSON SHTKContentChange    = Number 3

instance FromJSON SignatureHelpTriggerKind where
  parseJSON (Number 1) = pure SHTKInvoked
  parseJSON (Number 2) = pure SHTKTriggerCharacter
  parseJSON (Number 3) = pure SHTKContentChange
  parseJSON _          = fail "SignatureHelpTriggerKind"

-- | Additional information about the context in which a signature help request
-- was triggered.
--
-- @since 3.15.0
data SignatureHelpContext = 
  SignatureHelpContext
    { -- | Action that caused signature help to be triggered.
      _triggerKind :: SignatureHelpTriggerKind
      -- | Character that caused signature help to be triggered. This is
      -- undefined when @triggerKind !==
      -- SignatureHelpTriggerKind.TriggerCharacter@
    , _triggerCharacter :: Maybe Text
      -- | 'True' if signature help was already showing when it was triggered.
      -- 
      -- Retriggers occur when the signature help is already active and can be
      -- caused by actions such as typing a trigger character, a cursor move, or
      -- document content changes.
    , _isRetrigger :: Bool
      -- | The currently active 'SignatureHelp'.
      -- 
      -- The '_activeSignatureHelp' has its @SignatureHelp.activeSignature@
      -- field updated based on the user navigating through available
      -- signatures.
    , _activeSignatureHelp :: Maybe SignatureHelp
    }
  deriving (Read,Show,Eq)
deriveJSON lspOptions ''SignatureHelpContext

makeExtendingDatatype "SignatureHelpParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  ]
  [ ("_context", [t| Maybe SignatureHelpContext |])
  ]
deriveJSON lspOptions ''SignatureHelpParams


