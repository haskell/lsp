# Revision history for haskell-lsp-types

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

## 0.8.3.0

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

## 0.3.0.0

* Handle TextDocumentSync fallbacks with new TDS type.

## 0.2.3.0

* GHC 8.4.3 support
* Introduce additional error codes as per the LSP spec. By @Bubba

## 0.2.2.0  -- 2018-05-04

* Make Diagnostic relatedInformation optional, as per the LSP Spec. By @Bubba.

## 0.2.1.0  -- 2018-05-02

* Broken out from the haskell-lsp package, to simplify development
  by not having to run massive TH processes when working on the
  framework.
