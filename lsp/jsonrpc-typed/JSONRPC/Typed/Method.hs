{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module JSONRPC.Typed.Method where

import Control.Monad
import Data.Aeson qualified as A
import Data.GADT.Compare
import Data.Kind
import Data.Singletons
import Data.Traversable (for)
import JSONRPC.Method qualified as Untyped
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Datatype qualified as TH

-- | States that the constraint holds for all elements in the list.
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- | States that a type consists of an exhaustive list of elements.
class Closed k where
  type Elements k :: [k]

  -- | States that if a constraint holds for all elements in the type, then
  -- if we have a singleton for a value, we can use the constraint at that value.
  bring :: k `Everywhere` constr => proxy k -> Sing m -> (constr m => r) -> r

type Everywhere k (constr :: k -> Constraint) = All constr (Elements k)

-- | Which side of the server-client relationship are we taking?
data Role = Server | Client
  deriving stock (Show, Eq, Ord)

-- | Singleton for 'Role'.
data SRole (r :: Role) where
  SServer :: SRole Server
  SClient :: SRole Client

type instance Sing = SRole

-- | Maps a 'Role' to the other role.
type family OtherRole (r :: Role) where
  OtherRole Server = Client
  OtherRole Client = Server

-- | Singleton funtion for 'OtherRole'.
sOtherRole :: SRole r -> SRole (OtherRole r)
sOtherRole SServer = SClient
sOtherRole SClient = SServer

instance SingKind Role where
  type Demote Role = Role
  fromSing = \case
    SServer -> Server
    SClient -> Client
  toSing = \case
    Server -> SomeSing SServer
    Client -> SomeSing SClient

-- | The type of a method, either a request or a notification.
data RequestOrNotification = Request | Notification

data SRequestOrNotification k where
  SRequest :: SRequestOrNotification Request
  SNotification :: SRequestOrNotification Notification

type instance Sing = SRequestOrNotification

-- | The initiator of a method, either the client, the server, or either.
data Initiator = ServerInitiates | ClientInitiates | EitherInitiates

data SInitiator (i :: Initiator) where
  SServerInitiates :: SInitiator ServerInitiates
  SClientInitiates :: SInitiator ClientInitiates
  SEitherInitiates :: SInitiator EitherInitiates
type instance Sing = SInitiator

-- | Constraint bundle for use with 'Everywhere'/'bring'.
class
  ( A.ToJSON (MethodParams m)
  , A.FromJSON (MethodParams m)
  , A.ToJSON (MethodResult m)
  , A.FromJSON (MethodResult m)
  , A.ToJSON (ErrorData m)
  , A.FromJSON (ErrorData m)
  ) =>
  IsJsonParts m

instance
  ( A.ToJSON (MethodParams m)
  , A.FromJSON (MethodParams m)
  , A.ToJSON (MethodResult m)
  , A.FromJSON (MethodResult m)
  , A.ToJSON (ErrorData m)
  , A.FromJSON (ErrorData m)
  ) =>
  IsJsonParts m

-- | Constraint bundle for use with 'Everywhere'/'bring'.
class
  ( Eq (MethodParams m)
  , Eq (MethodResult m)
  , Eq (ErrorData m)
  ) =>
  EqParts m

instance
  ( Eq (MethodParams m)
  , Eq (MethodResult m)
  , Eq (ErrorData m)
  ) =>
  EqParts m

-- | Constraint bundle for use with 'Everywhere'/'bring'.
class
  ( Show (MethodParams m)
  , Show (MethodResult m)
  , Show (ErrorData m)
  ) =>
  ShowParts m

instance
  ( Show (MethodParams m)
  , Show (MethodResult m)
  , Show (ErrorData m)
  ) =>
  ShowParts m

{- | Everything that we need to treat a given type as a type of methods.

Broadly, we need:

    * closure over the type
    * singletons for the methods
    * type families picking out the various aspects of the various
      methods
    * 'singled' functions for most of those type families
    * useful instances for the corresponding types
-}
class
  ( SingKind k
  , Demote k ~ k
  , Show k
  , Eq k
  , GCompare @k Sing
  , Closed k
  , k `Everywhere` IsJsonParts
  , k `Everywhere` EqParts
  , k `Everywhere` ShowParts
  ) =>
  Method k
  where
  fromUntypedMethod :: Untyped.Method -> Maybe k
  toUntypedMethod :: k -> Untyped.Method

  type MethodInitiator (m :: k) :: Initiator
  sMethodInitiator :: forall (m :: k). Sing m -> Sing (MethodInitiator m)

  type MethodType (m :: k) :: RequestOrNotification
  sMethodType :: forall (m :: k). Sing m -> Sing (MethodType m)

  type MethodParams (m :: k) :: Type
  type MethodResult (m :: k) :: Type
  type ErrorData (m :: k) :: Type

-- | A boolean singleton, just to avoid depending on @singletons-base@ to get @SBool@.
data Allowed = Yes | No

data SAllowed (a :: Allowed) where
  SYes :: SAllowed Yes
  SNo :: SAllowed No

type instance Sing = SAllowed

-- | Given a role and an an initiator status, is it allowed to send the given method?
type family CanInitiate (r :: Role) (i :: Initiator) :: Allowed where
  CanInitiate Server ServerInitiates = Yes
  CanInitiate Server ClientInitiates = No
  CanInitiate Server EitherInitiates = Yes
  CanInitiate Client ServerInitiates = No
  CanInitiate Client ClientInitiates = Yes
  CanInitiate Client EitherInitiates = Yes

sCanInitiate :: Sing r -> Sing i -> Sing (CanInitiate r i)
sCanInitiate SServer SServerInitiates = SYes
sCanInitiate SServer SClientInitiates = SNo
sCanInitiate SServer SEitherInitiates = SYes
sCanInitiate SClient SServerInitiates = SNo
sCanInitiate SClient SClientInitiates = SYes
sCanInitiate SClient SEitherInitiates = SYes

{- | Constraint that packs up everything we need to check whether a given role is allowed to send a
given method as a notification.
-}
type NotificationOk r m = (CanInitiate r (MethodInitiator m) ~ Yes, MethodType m ~ Notification)

{- | Constraint that packs up everything we need to check whether a given role is allowed to send a
given method as a request.
-}
type RequestOk r m = (CanInitiate r (MethodInitiator m) ~ Yes, MethodType m ~ Request)

{- | Constraint that packs up everything we need to check whether a given role is allowed to send a
given method as a response.
-}
type ResponseOk r m = (CanInitiate (OtherRole r) (MethodInitiator m) ~ Yes, MethodType m ~ Request)

deriveClosed :: TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveClosed tyName sTyName = do
  TH.DatatypeInfo{datatypeCons = cons} <- TH.reifyDatatype tyName
  TH.DatatypeInfo{datatypeCons = sCons} <- TH.reifyDatatype sTyName
  sn <- TH.newName "s"
  kn <- TH.newName "k"

  matches <- for sCons $ \TH.ConstructorInfo{constructorName = cn, constructorFields = flds} -> do
    unless (null flds) $ fail "All constructors must have no arguments"
    pure $ TH.Match (TH.ConP cn [] []) (TH.NormalB $ TH.VarE kn) []
  let kase = TH.CaseE (TH.VarE sn) matches
      clause = TH.Clause [TH.WildP, TH.VarP sn, TH.VarP kn] (TH.NormalB kase) []
      bringDecl = TH.FunD 'bring [clause]

  refs <- for cons $ \TH.ConstructorInfo{constructorName = cn, constructorFields = flds} -> do
    unless (null flds) $ fail "All constructors must have no arguments"
    pure $ TH.ConT cn
  let allTypes = foldr (\ty tyl -> TH.PromotedConsT `TH.AppT` ty `TH.AppT` tyl) TH.PromotedNilT refs
      elementsDecl = TH.TySynInstD $ TH.TySynEqn Nothing (TH.ConT ''Elements `TH.AppT` TH.ConT tyName) allTypes

  let closedDecl = TH.InstanceD Nothing [] (TH.ConT ''Closed `TH.AppT` TH.ConT tyName) [elementsDecl, bringDecl]

  pure [closedDecl]
