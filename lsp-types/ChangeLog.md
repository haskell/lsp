# Revision history for lsp-types
 
## 2.3.0.0 -- 2024-06-06

- Add support for identifying client and server capabilities associated with a method.
- `TResponseMessage` now contains a `TResponseError` instead of a `ResponseError`

## 2.2.0.0 -- 2024-04-29

- Update the metamodel. This results in a number of breaking changes to the generated code, mostly replacing
  anonymous structs with named ones.

## 2.1.1.0 -- 2024-02-24

- Require aeson 2
- Accept `null` in place of a missing field, a common mistake in spec compliance.
- Arbitrary instances for everything now exist and are exported via the new `lsp-types-quickcheck` library

## 2.1.0.0 -- 2023-11-13

- Add `dynamicRegistrationSupported` to `Capabilities`.
- Fully update `fullCaps` for recent spec versions.
- Support GHC 9.8, drop checked support for GHC 8.10

## 2.0.2.0 -- 2023-08-24

- Add `Language.LSP.Protocol.Utils.Misc.prettyJSON :: Value -> Doc ann` for prettyprinting JSON,
  and `ViaJSON` as a deriving-via newtype wrapper for it.
- Generate `Pretty` instances for all LSP types using `ViaJSON`.

## 2.0.1.1 -- 2023-08-08

* Fix parsing of notifications with missing params

## 2.0.1.0 -- 2023-07-14

* Removed deprecation pragmas from fields, as these cannot currently be avoided.
* Added `isOptionalMethod`, that checks whether a method is optional according to the spec.

## 2.0.0.1 -- 2023-06-16

* Fixed missing `extra-source-files` for `metamodel.json`.

## 2.0.0.0 -- 2023-06-14

* Breaking change: major restructure to generate types and methods from the LSP metamodel.
    * Full support for version 3.17 of the LSP specification, many accuracy fixes
    * Generated types follow the spec very closely, e.g. using anonymous types, using `a |? Null` instead of `Maybe a`
    * Anonymous record types in the spec are now represented using `row-types`
    * Many constructors are now prefixed with their type names
    * Documentation from the spec is transferred
    * Three top level modules: `Types` (main protocol types), `Message` (messages and methods), `Capabilities` (capabilities)
* New typeclasses for handling LSP enumerations: `LspEnum` and `LspOpenEnum`

## 1.6.0.1 -- 2023-12-02

* Add `LANGUAGE DuplicateRecordFields` to facilitate building with GHC 9.8

## 1.6.0.0 -- 2022-09-13

* Add `isSubRangeOf` and `positionInRange` helper functions
* Add `ServerCancelled`, `RequestFailed` and `ErrorCodeCustom` server error types
* Fix "workspace/semanticTokens/refresh" to be a server method instead of a client method
* Use a packed representation for `NormalizedFilePath`
* Add conversions from `OsPath` to `NormalizedFilePath` in `Language.LSP.Types.Uri.OsPath` when using new enough `filepath`

## 1.5.0.0 -- 2022-06-20

* VFS module moved from `lsp-types` to `lsp`, as it relates to the actual implementation of a LSP server.

## 1.4.0.1 -- 2022-01-20

* Fix result type of selection range requests.

## 1.4.0.0 -- 2021-12-28

* Aeson 2 compatibility (#360) (@michaelpj)
* Reduced dependency footprint (#383, #384) (@Bodigrim)
* Use appropriate number types (#366) (@michaelpj)
* Fix for #374 (#376) (@pepeiborra)
* Fix virtual file name adding .hs extension (#364) (@heitor-lassarote, @jneira)
* Fix the Semigroup instance for MarkupContent (#361) (@michaelpj)
* Various improvements to spec conformance (@michaelpj)

## 1.3.0.1 -- 2021-08-06

* Rollback NFP interning (#344) (@pepeiborra)

## 1.3.0.0 -- 2021-07-31

* Intern NormalizedFilePaths (#340) (@pepeiborra)

## 1.2.0.1 -- unpublished

* Add compatibility with GHC 9.2 (#345) (@fendor)
* Fix missing lenses (@michaelpj)
* Semantic tokens support (@michaelpj)
* Fix GHC 9 build (@anka-213)
* Add call hierarchy support (@July541)
* Do not crash on workspace/didChangeConfiguration (#321) (@strager)
* Improve error messages on JSON decode failures (#320) (@strager)

## 1.2.0.0 -- 2021-03-28

* Prevent crashing when optional fields are missing (@anka-213)
* Use StrictData (@wz1000)
* Add MarkupContent in SignatureHelp types (@michaelpj)
* Add activeParameter support in SignatureInformation (@michaelpj)
* Add label offset support in SignatureHelp (@michaelpj)
* Add some documentation comments to SignatureHelp types (@michaelpj)
* Add support for InsertReplaceEdit (@michaelpj)
* Add support for InsertTextMode (@michaelpj)
* Add resolveSupport (@michaelpj)
* Fix applying a TextEdit past the end of the document (#271) (@michaelpj)
* Use Empty instead of () as progress create response (#295) (@wz1000)
* Add tag support for DocumentSymbol, SymbolInformation, and document symbol provider label (#301) (@michaelpj)
* Support change annotations (#302) (@michaelpj)
* Add some more missing lenses (#307) (@michaelpj)

## 1.1.0.0 -- 2021-02-14

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

## 1.0.0.0 -- 2020-10-15

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

## 0.23.0.0 -- 2020-10-05

* Add runWith for transporots other than stdio (@paulyoung)
* Fix race condition in event captures (@bgamari)
* Tweak the sectionSeparator (@alanz)
* Add hashWithSaltInstances (@ndmitchell)
* Fix CompletionItem.tags not being optional (@bubba)
* Avoid unnecessary normalisation in Binary instance for
  NormalizedFilePath (@cocreature)
* Fix ordering of TH splices (@fendor)

## 0.22.0.0 -- 2020-05-04

* ResponseMessage results are now an Either type (@greenhat)
* Support for GHC 8.10.1

## 0.21.0.0 -- 2020-03-21

* Stop getCompletionPrefix from crashing if beforePos is empty
* Add DidChangeWatchedFilesRegistrationOptions
* Add NormalizedFilePath from ghcide
* Add diagnostic and completion tags
* Fix language server example
* Correctly fix the problem with '$/' notifications
* Add azure ci

## 0.20.0.0 -- 2020-02-04T

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

* Fix response type for CodeLensResolve, add the ContentModified error
  code (@SquidDev)
* Add missing fmClientPrepareRenameRequest to MessageFuncs export (@alanz)
* Rework Core.Options and infer all server capabilities from handlers (@bubba)
* Generate lenses for WorkDoneProgress data types (@alanz)

## 0.17.0.0 -- 2019-10-18

* Update progress reporting to match the LSP 3.15 specification (@cocreature)
* Ensure ResponseMessage has either a result or an error (@cocreature)

## 0.16.0.0 -- 2019-09-07

* Add support for CodeActionOptions (@thomasjm)
* Add support for `textDocument/prepareRename` request (@thomasjm)
* Fix diagnostic code parsing (@thomasjm)
* Fix shutdown response type (@bubba)
* Relax base constraints for GHC 8.8 (@bubba)

## 0.15.0.0 -- 2019-07-01

* Normalize URIs to avoid issues with percent encoding (@cocreature)

## 0.14.0.1 -- 2019-06-13

* Fix Haddock error

## 0.14.0.0 -- 2019-06-13

* Add support for custom request and notification methods
  (@cocreature)

## 0.11.0.0 -- 2019-04-28

* Fix `window/progress/cancel` notification being a from server
  notification when it should be a from client notification
* Fix typo in FoldingRange request name

## 0.10.0.0 -- 2019-04-22

* Add types for `window/progress` notifications.

## 0.8.3.0 -- unpublished

* Add `MarkupContent` to `HoverResponse`, and (some) json roundtrip tests.

## 0.8.2.0 -- 2019-04-11

* Add `applyTextEdit` and `editTextEdit` helpers

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

## 0.6.0.0  -- 2018-08-06

* Add new DocumentSymbol type and hierarchal support
* Rename CommandOrCodeAction to CAResult

## 0.5.0.0  -- 2018-08-03

* Update Command.arguments to match specification
* Update ClientCapabilities to v3.10
* Add MarkupContent
* Add new CompletionKinds
* Add new SymbolKinds
* Add preset version capabilities

## 0.4.0.0  -- 2018-07-10

* CodeAction support as per v3.8 of the specification, by @Bubba
* Update VersionedTextDocumentIdentifier to match specification, by @Bubba.

## 0.3.0.0 -- unpublished

* Handle TextDocumentSync fallbacks with new TDS type.

## 0.2.3.0 -- unpublished

* GHC 8.4.3 support
* Introduce additional error codes as per the LSP spec. By @Bubba

## 0.2.2.0  -- 2018-05-04

* Make Diagnostic relatedInformation optional, as per the LSP Spec. By @Bubba.

## 0.2.1.0  -- 2018-05-02

* Broken out from the haskell-lsp package, to simplify development
  by not having to run massive TH processes when working on the
  framework.
