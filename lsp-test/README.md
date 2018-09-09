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
To test make sure you have the following language servers installed:
- [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
- [javascript-typescript-langserver](https://github.com/sourcegraph/javascript-typescript-langserver)
