# Revision history for lsp-test

## 0.13.0.0 -- 2021-03-26

* Update for lsp-types-1.2 (@wz1000)
* Limit diagnostics by range in getCodeActions (@aufarg)
* Kill timeout thread to avoid building up thousands of old TimeoutMessages (@wz1000)

## 0.13.0.0 -- 2021-02-14

* Add support for lsp-types-1.1 (@wz1000)
* Automatically respond to `workspace/applyEdit` and
  `window/workDoneProgress/create` messages (@wz1000)

## 0.12.0.0 -- 2021-01-25
* Add `getIncompleteProgressSessions` to track ongoing progress sessions
  (@wz1000)

## 0.11.0.0 -- 2020-05-14

* Replace `openDoc'` with `createDoc` which now sends
  `workspace/didChangeWatchedFiles` notifications if the server has registered
  for it
* Add `getRegisteredCapabilities`

## 0.10.3.0 -- 2020-05-04

* Build with new haskell-lsp-0.22

## 0.10.2.0 -- 2020-03-21

* Bump constraints for new haskell-lsp

## 0.10.1.0 -- 2020-02-04

* Bump constraints for new haskell-lsp

## 0.10.0.0 -- 2019-12-29

* Account for messages received between the initialize request and response.
  (Though it will throw an exception if the message received is an illegal one)

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
