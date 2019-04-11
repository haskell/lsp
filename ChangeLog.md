# Revision history for haskell-lsp

## 0.8.2.0 -- 2019-04-11

* Add `applyTextEdit` and `editTextEdit` helpers

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
