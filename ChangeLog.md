# Revision history for haskell-lsp

## 0.5.0.0  -- 2018-xx-xx

* Update Command.arguments to match specification
* Update ClientCapabilities to v3.10
* Add MarkupContent
* Add new CompletionKinds
* Add new DocumentSymbol type and heirarchal support
* Rename CommandOrCodeAction to CAResult
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
