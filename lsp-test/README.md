# lsp-test [![Build Status](https://travis-ci.com/bubba/lsp-test.svg?branch=master)](https://travis-ci.com/bubba/lsp-test) [![Hackage](https://img.shields.io/hackage/v/lsp-test.svg)](https://hackage.haskell.org/package/lsp-test-0.1.0.0)
lsp-test is a functional testing framework for Language Server Protocol servers.

```haskell
import Language.Haskell.LSP.Test
runSession "hie" fullCaps "proj/dir" $ do
  doc <- openDoc "Foo.hs" "haskell"
  skipMany anyNotification
  symbols <- getDocumentSymbols doc
```

## Examples

### Unit tests with HSpec
```haskell
describe "diagnostics" $
  it "report errors" $ runSession "hie" fullCaps "test/data" $ do
    openDoc "Error.hs" "haskell"
    [diag] <- waitForDiagnosticsSource "ghcmod"
    liftIO $ do
      diag ^. severity `shouldBe` Just DsError
      diag ^. source `shouldBe` Just "ghcmod"
```

### Replaying captured session
```haskell
replaySession "hie" "test/data/renamePass"
```

### Parsing with combinators
```haskell
skipManyTill loggingNotification publishDiagnosticsNotification
count 4 (message :: Session ApplyWorkspaceEditRequest)
anyRequest <|> anyResponse
```

Try out the example tests in the `example` directory with `cabal new-test`.
For more examples check the [Wiki](https://github.com/bubba/lsp-test/wiki/Introduction)

## Developing
The tests are integration tests, so make sure you have the following language servers installed and on your PATH:
### [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
- Check out a relatively recent version of the repo, or see `.travis.yml` to get the exact commit used for CI.
- `stack install`
### [javascript-typescript-langserver](https://github.com/sourcegraph/javascript-typescript-langserver)
`npm i -g javascript-typescript-langserver`

Then run the tests with `stack test` or `cabal new-test`.

## Troubleshooting
Seeing funny stuff when running lsp-test via stack? If your server is built upon Haskell tooling, [keep in mind that stack sets some environment variables related to GHC, and you may want to unset them.](https://github.com/alanz/haskell-ide-engine/blob/bfb16324d396da71000ef81d51acbebbdaa854ab/test/utils/TestUtils.hs#L290-L298)
