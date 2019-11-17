# Revision history for haskell-lsp

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
