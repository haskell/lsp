# haskell-lsp-test [![Build Status](https://travis-ci.com/Bubba/haskell-lsp-test.svg?branch=master)](https://travis-ci.com/Bubba/haskell-lsp-test)
haskell-lsp-test is a functional testing framework for Language Server Protocol servers.

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
replaySession "hie --lsp" "test/data/renamePass"
```

### Parsing with combinators
```haskell
skipManyTill loggingNotification publishDiagnosticsNotification
count 4 (message :: Session ApplyWorkspaceEditRequest)
anyRequest <|> anyResponse
```

For more examples check the [Wiki](https://github.com/Bubba/haskell-lsp-test/wiki/Introduction)

## Developing
To test make sure you have the following language servers installed:
- [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
- [javascript-typescript-langserver](https://github.com/sourcegraph/javascript-typescript-langserver)
