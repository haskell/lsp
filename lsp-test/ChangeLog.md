# Revision history for lsp-test

## 0.9.0.0 -- 2019-12-1

* Add `ignoreLogNotifications` config option
* Add ability to override `logStdErr` and `logMessages` config options with
  the `LSP_TEST_LOG_STDERR` and `LOG_TEST_LOG_MESSAGES` environment variables
* Update for haskell-lsp-0.19.0.0 (@mpickering)

## 0.8.2.0 -- 2019-11-17

* Expose `satisfyMaybe` (@cocreature)

## 0.8.1.0 -- 2019-11-17

* Update to haskell-lsp-0.18.0.0 (@mpickering, @alanz)
* Tests now require hie-bios based hie

## 0.8.0.0 -- 2019-10-18

* Make `Session` a newtype
* Update for haskell-lsp-0.17.0.0 (@cocreature)

## 0.7.0.0 -- 2019-09-08

* Update for haskell-lsp-0.16.0.0

## 0.6.1.0 -- 2019-08-24

* Add `satisfyMaybe` (@cocreature)

## 0.6.0.0 -- 2019-07-04

* Update to haskell-lsp-0.15.0.0 (@lorenzo)

## 0.5.4.0 -- 2019-06-13

* Fix `getDefinitions` for SingleLoc (@cocreature)
* Add `getCodeLenses` (@cocreature)

## 0.5.3.0 -- 2019-06-13

* Update to haskell-lsp-0.14.0.0 (@cocreature)
* Support `TextDocumentDidChange` (@cocreature)
* Add non-file based `openDoc` (@cocreature)

## 0.5.2.0 -- 2019-04-28

* Add `satisfy` parser combinator

## 0.5.1.0 -- 2019-04-22

* Fix unhandled `window/progress` server notifications

## 0.5.1.0 -- 2019-04-07

* Add getTypeDefinitions (@fendor)

## 0.5.0.2 -- 2018-12-05

* Fix loose threads when exceptions are thrown

## 0.5.0.0 -- 2018-11-13

* Add lspConfig option in config
* GHC 8.6.2 support

## 0.4.0.0 -- 2018-09-08

* Update to haskell-lsp-0.8.0.0

## 0.3.0.0 -- 2018-09-0t

* Update to haskell-lsp-0.7.0.0

## 0.2.1.0 -- 2018-08-14

* Add getCodeActions
* Add getCurrentDiagnostics

## 0.2.0.0 -- 2018-08-06

* Update to haskell-lsp 0.6.0.0
