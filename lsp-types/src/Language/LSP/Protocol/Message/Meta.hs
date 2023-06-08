module Language.LSP.Protocol.Message.Meta where

-- | Which direction messages are sent in.
data MessageDirection = ServerToClient | ClientToServer
-- | What kind of message is sent.
data MessageKind = Notification | Request

-- | Singleton type for 'MessageDirection'.
data SMessageDirection (f :: MessageDirection) where
  SClientToServer :: SMessageDirection ClientToServer
  SServerToClient :: SMessageDirection ServerToClient
  SBothDirections :: SMessageDirection f

-- | Singleton type for 'MessageKind'.
data SMessageKind (f :: MessageKind) where
  SNotification :: SMessageKind Notification
  SRequest :: SMessageKind Request
  SBothTypes :: SMessageKind f
