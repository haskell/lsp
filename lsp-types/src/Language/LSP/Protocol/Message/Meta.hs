module Language.LSP.Protocol.Message.Meta where

-- | Which direction messages are sent in.
data Initiator = ServerInitiates | ClientInitiates | EitherInitiates

-- | What kind of message is sent.
data RequestOrNotification = Notification | Request

-- | Singleton type for 'Initiator'.
data SInitiator (f :: Initiator) where
  SServerInitiates :: SInitiator ServerInitiates
  SClientInitiates :: SInitiator ClientInitiates
  SEitherInitiates :: SInitiator EitherInitiates

-- | Singleton type for 'RequestOrNotification'.
data SRequestOrNotification (f :: RequestOrNotification) where
  SNotification :: SRequestOrNotification Notification
  SRequest :: SRequestOrNotification Request
