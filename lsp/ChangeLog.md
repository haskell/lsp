# Revision history for lsp

## 1.2.0.0

* Parse config from initializeOptions and pass in the old value of config to onConfigurationChange (#285) (wz1000)
* Break up big TVar record into lots of small TVars (#286) (@wz1000)
* Add some INLINE pragmas (@wz1000)
* Make method equality work for CustomMethods (@wz1000)

## 1.1.1.0

* Don't send begin progress notification twice (@wz1000)

## 1.1.0.0

* Fix prepareRename reponse and prepareProvider (@kirelagin)
* Fix deriving instance of MonadUnliftIO (@banacorn)
* Add support for file and folder operations in WorkspaceEdit (@banacorn)
Instead of having TextDocumentEdit in WorkspaceEdit

```haskell
data WorkspaceEdit =
  WorkspaceEdit
    { _changes         :: Maybe WorkspaceEditMap
    , _documentChanges :: Maybe (List TextDocumentEdit)
    } deriving (Show, Read, Eq)
```
It is now replaced by a new type called DocumentChange

```haskell
data WorkspaceEdit =
  WorkspaceEdit
    { _changes         :: Maybe WorkspaceEditMap
    , _documentChanges :: Maybe (List DocumentChange)
    } deriving (Show, Read, Eq)
```
Which is just a synonym of union of WorkspaceEdit and other operations

```haskell
type DocumentChange = TextDocumentEdit |? CreateFile |? RenameFile |? DeleteFile
```
* Add new CodeAction features (isPreferred, disabled) (@pepeiborra)
* Respond to requests with missing handlers (@wz1000)
* Use Text over String in more places (@wz1000)
* Add missing lenses (@wz1000, @bubba)

## 1.0.0.0

1.0.0.0 is a major rework with both internal and external facing changes, and
will require manual migration.

* The package has been renamed from `haskell-lsp` to `lsp`, and similarly for `haskell-lsp-types` to `lsp-types`
  * Because of this, all modules are now exported from `Language.LSP.X` rather than `Language.Haskell.X`.
* Both `lsp` and `lsp-types` have been reworked to be much more *type safe*
* The 3.15 specification should be fully supported now. If you find anything in
  the specification that isn't in lsp-types, please let us know
* The Capture module has been removed as it will be reworked later on and moved to lsp-test
* `lsp` can now handle dynamic registration through the `registerCapability` and
  `unregisterCapability` functions

### Type safety

There are three types of concrete messages, `NotificationMessage`,
`RequestMessage` and `ResponseMessage`. They are parameterised by their
`Method`, which determines what type their parameters or response result must be.

```haskell
data RequestMessage (m :: Method f Request) = RequestMessage
    { _jsonrpc :: Text
    , _id      :: LspId m
    , _method  :: SMethod m
    , _params  :: MessageParams m
    }
```

A `Method` in turn is parameterised by whether it originates from the client or
the server, and whether it is used for notifications or requests:

```haskell
TextDocumentFoldingRange           :: Method FromClient Request
TextDocumentSelectionRange         :: Method FromClient Request
WindowShowMessage                  :: Method FromServer Notification
WindowShowMessageRequest           :: Method FromServer Request
```

Each `Method` also has a singleton counterpart which allows it to be used at the
term level, for example in `RequestMessage._method`:

```haskell
STextDocumentFoldingRange           :: SMethod TextDocumentFoldingRange
STextDocumentSelectionRange         :: SMethod TextDocumentSelectionRange

SWindowShowMessage                  :: SMethod WindowShowMessage
SWindowShowMessageRequest           :: SMethod WindowShowMessageRequest
```

The type families `MessageParams` and `ResponseResult` map each `Method` to the
appropriate type to be used in a response:

```haskell
ResponseResult TextDocumentRename            = WorkspaceEdit
ResponseResult TextDocumentPrepareRename     = Range |? RangeWithPlaceholder
```

Also new is the `|?` type which represents [union types in
TypeScript](https://www.typescriptlang.org/docs/handbook/unions-and-intersections.html#union-types),
and is used throughout the specification where a field can accept several
different types.

As an example of this in action, the types of your handlers will now depend on
whether or not they are a request or a notification. They will pass along the
precise type for the parameters the method you are handling, and in the case of
a request handler, will expect that the response you give back is of the correct
type as well.

```haskell
type family Handler (f :: Type -> Type) (m :: Method from t) = (result :: Type) | result -> f t m where
  Handler f (m :: Method _from Request)      = RequestMessage m -> (Either ResponseError (ResponseResult m) -> f ()) -> f ()
  Handler f (m :: Method _from Notification) = NotificationMessage m -> f ()
```

### LspT
`LspFuncs` has been removed and instead functionality is exposed through
functions in the `MonadLsp` class.

```haskell
getVirtualFile :: MonadLsp config m => NormalizedUri -> m (Maybe VirtualFile)
sendRequest :: forall (m :: Method FromServer Request) f config. MonadLsp config f
            => SServerMethod m
            -> MessageParams m
            -> (Either ResponseError (ResponseResult m) -> f ())
            -> f (LspId m)
```

It is parameterised over the server's LSP configuration type and the underlying
monad.
We recommend that you build your own monad for your server on top of the `LspT`
transformer, so it will automatically become an instance of `MonadLsp`.

Inside the new `ServerDefinition` data type which gets passed to `runServer`,
you need to specify how to convert from IO to your monad and back in
`interpretHandler` so that `lsp` can execute your monad inside the handlers. You
can use the result returned from `doInitialize` to pass along the
`LanguageContextEnv` needed to run an `LspT`, as well as anything else your
monad needs.
```haskell
type 
ServerDefinition { ...
, doInitialize = \env _req -> pure $ Right env
, interpretHandler = \env -> Iso 
   (runLspT env) -- how to convert from IO ~> m
   liftIO        -- how to convert from m ~> IO
}
```

### Steps to migrate

1. In your `.cabal` file change any `haskell-lsp` dependencies to `lsp`
2. Replace your existing imports with `Haskell.LSP.Server`
3. If necessary define your own monad and fill in `interpretHandler`
4. Migrate your handlers to use `notificationHandler` and `requestHandler`,
   passing along the corresponding `SMethod` (See `example/Simple.hs`)
5. Remove any storage/use of `LspFuncs` and instead call the corresponding
   functions directly from your monad instead of `IO`

## 0.23.0.0

* Add runWith for transporots other than stdio (@paulyoung)
* Fix race condition in event captures (@bgamari)
* Tweak the sectionSeparator (@alanz)
* Add hashWithSaltInstances (@ndmitchell)
* Fix CompletionItem.tags not being optional (@bubba)
* Avoid unnecessary normalisation in Binary instance for
  NormalizedFilePath (@cocreature)
* Fix ordering of TH splices (@fendor)

## 0.22.0.0

* ResponseMessage results are now an Either type (@greenhat)
* Support for GHC 8.10.1

## 0.21.0.0

* Stop getCompletionPrefix from crashing if beforePos is empty
* Add DidChangeWatchedFilesRegistrationOptions
* Add NormalizedFilePath from ghcide
* Add diagnostic and completion tags
* Fix language server example
* Correctly fix the problem with '$/' notifications
* Add azure ci

## 0.20.0.1

* Fix Haddock generation syntax error

## 0.20.0.0

* Force utf8 encoding when writing vfs temp files
* Don't log errors for '$/' notifications (@jinwoo)
* Force utf8 encoding when writing vfs temp files (@jneira)
* Store a hash in a NormalizedUri (@mpickering)
* Move "Semigroup WorkspaceEdit" instance (@sheaf)
* Fix vfs line endings (@jneira)

## 0.19.0.0 -- 2019-12-14

* Fix vfs line endings (@jneira)
* Fix typo in .cabal (@turion)
* Normalize file paths before converting to Uri's (@jneira)
* Fixes to persistVirtualFile (@mpickering)

## 0.18.0.0 -- 2019-11-17

* Explain the use of NonEmpty in
  documentOnTypeFormattingTriggerCharacters (@bubba)
* Fix response type for CodeLensResolve, add the ContentModified error
  code (@SquidDev)
* Virtual file fixes, removing race conditions and other cleanups (@mpickering)
* Add missing fmClientPrepareRenameRequest to MessageFuncs export (@alanz)
* Make explicit GHC 8.6.5 stack file (@alanz)
* Rework Core.Options and infer all server capabilities from handlers (@bubba)
* Generate lenses for WorkDoneProgress data types (@alanz)

## 0.17.0.0 -- 2019-10-18

* Update progress reporting to match the LSP 3.15 specification (@cocreature)
* Fix progress cancellation action being retained (@mpickering)
* Respect both codeActionProvider and codeActionHandler in server
  capabilities (@fendor)
* Ensure ResponseMessage has either a result or an error (@cocreature)

## 0.16.0.0 -- 2019-09-07

* Add support for CodeActionOptions (@thomasjm)
* Add support for `textDocument/prepareRename` request (@thomasjm)
* Fix diagnostic code parsing (@thomasjm)
* Fix shutdown response type (@bubba)
* Relax base constraints for GHC 8.8 (@bubba)

## 0.15.0.0 -- 2019-07-01

* Fix decoding of `ResponseMessage` to account for `null` messages (@cocreature)
* Normalize URIs to avoid issues with percent encoding (@cocreature)
* Changed the initial callbacks type to also capture initial config (@lorenzo)
* Improved documentation (@bubba)

## 0.14.0.0 -- 2019-06-13

* Add support for custom request and notification methods
  (@cocreature)
* Use attoparsec to parse message headers incrementally (@cocreature)
* Only build lsp-hello when -fdemo flag is set (@bubba)

## 0.13.0.0 -- 2019-05-18

* Fix relative posix URIs (@DavidM-D)
* Make sure that markedUpContent always starts on a newline (@alanz)

## 0.12.1.0 -- 2019-05-08

* Bring over functions from @mpickering's hie-bios.
  So `LspFuncs` now includes

```haskell
    , persistVirtualFileFunc       :: !(J.Uri -> IO FilePath)
    , reverseFileMapFunc           :: !(IO (FilePath -> FilePath))
```

* Fix exception on empty filepaths

* Migrate some utility functions from `haskell-ide-engine`, for the
  benefit of other language servers.
  - `rangeLinesFromVfs`
  - `PosPrefixInfo(..)`
  - `getCompletionPrefix`

* Remove `HoverContentsEmpty`. It is unnecessary, and generated
  illegal JSON on the wire.

## 0.12.0.0 -- 2019-05-05

* Added A NFData instance for Diagnostics (@DavidM-D/@ndmitchell)
* Switch to using the rope-utf16-splay library for ropes (@ollef)

## 0.11.0.0 -- 2019-04-28

* Add support for cancellable requests within `withProgress` and
  `withIndefiniteProgress`
* Align `withProgress` and `withIndefiniteProgress` types to be in `IO`
  like the rest of the library. (Look at using `monad-control` and
  `unliftio` if you need to use them with a Monad transformer stack)

## 0.10.0.0 -- 2019-04-22

* Add `withProgress` and `withIndefiniteProgress` functions for sending
  `window/progress` notifications.

## 0.9.0.0

* Add `MarkupContent` to `HoverResponse`, and (some) json roundtrip tests.

## 0.8.2.0 -- 2019-04-11

* Add `applyTextEdit` and `editTextEdit` helpers
* Set the typedefinitionProvider capability if it has a handler
* Add stack files for GHC 8.4.4 and 8.6.4

## 0.8.1.0 -- 2019-02-28

* Update Handler to delegate to typeDefinitionHandler instead of
  definitionHandler. by @fendor

## 0.8.0.1 -- 2018-10-27

* Support GHC 8.6.1 by loosening constraints. Via @domenkozar

## 0.8.0.0 -- 2018-09-08

* Update Hover to be a nullable according to spec
* Move Lenses into a separate module, `Language.Haskell.LSP.Types.Lens`

## 0.7.0.0 -- 2018-08-14

* Update CompletionItem
 * Add `commitCharacters` field
 * Add `MarkupContent` option for `documentation`
 * Add `preselect` field
* Add CompletionContext
* Add new server capabilities
* Add workspace folder support
* Add document color and color presentation
* Add folding range support
* Add goto type support
* s/TH/Types/g
 * Move all types into haskell-lsp-types
 * Hide Language.Haskell.LSP.TH.DataTypesJSON - Use Language.Haskell.LSP.Types instead
* Add lenses for Language.Haskell.LSP.Types.Capabilities


## 0.6.0.0 -- 2018-08-06

* Add new DocumentSymbol type and heirarchal support
* Rename CommandOrCodeAction to CAResult
* Add handler for 'textDocument/implementation' request from client
* Bump stack resolvers for lts 11 and lts 12

## 0.5.0.0  -- 2018-08-03

* Update Command.arguments to match specification
* Update ClientCapabilities to v3.10
* Add MarkupContent
* Add new CompletionKinds
* Add new SymbolKinds
* Add preset version capabilities

## 0.4.0.0  -- 2018-07-10

* CodeAction support as per v3.8 of the specification, by @Bubba
* Update VersionedTextDocumentIdentifier to match specification, by @Bubba

## 0.3.0.0

* Handle TextDocumentSync fallbacks with new TDS type.

## 0.2.3.0  -- 2018-99-99

* GHC 8.4.3 support
* Apply changes to the VFS in the order received in a message.
  This fixes vscode undo behaviour. By @Bubba
* Introduce additional error codes as per the LSP spec. By @Bubba
* Add preliminary support for recording LSP traffic for later playback
  in test scenarios. By @Bubba

## 0.2.2.0  -- 2018-05-04

* Make Diagnostic relatedInformation optional, as per the LSP Spec. By @Bubba.

## 0.2.1.0  -- 2018-05-02

* Support GHC 8.4.2
* Split into two packages
* Language.Haskell.LSP.TH.DataTypesJSON becomes Language.Haskell.LSP.Types
* Diagnostic now has _relatedInformation. Can default it to mempty. via @AlexeyRaga
* Correct the name of the DidChangeWatchedFilesParams field, by @robrix
* Make sure to escape URIs properly for Windows file paths
  Fixes #75. Also added a couple of pretty dumb tests!, by @johnsonw


## 0.2.0.1  -- 2017-12-27

* Built with LTS 10.1 (stack)
* Don't escape semicolons after drive letters by @nponeccop
* Add Foldable and Traversable instance to List by @noughtmare

## 0.2.0.0  -- 2017-11-23

* Major changes as implementation continued. Now seems stable, used in haskell-ide-engine

## 0.1.0.0  -- 2017-07-19

* First version. Implements version 3 of the Microsoft Language
  Server Protocol
