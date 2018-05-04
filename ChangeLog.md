# Revision history for haskell-lsp

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
