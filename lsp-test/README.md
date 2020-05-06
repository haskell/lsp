# lsp-test [![Actions Status](https://github.com/bubba/lsp-test/workflows/Haskell%20CI/badge.svg)](https://github.com/bubba/lsp-test/actions) [![Hackage](https://img.shields.io/hackage/v/lsp-test.svg)](https://hackage.haskell.org/package/lsp-test)
lsp-test is a functional testing framework for Language Server Protocol servers.

```haskell
import Language.Haskell.LSP.Test
main = runSession "hie" fullCaps "proj/dir" $ do
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

Try out the example tests in the `example` directory with `cabal test`.
For more examples check the [Wiki](https://github.com/bubba/lsp-test/wiki/Introduction)

## Developing
The tests for lsp-test use a dummy server found in `test/dummy-server/`.
Run the tests with `cabal test` or `stack test`.
Tip: If you want to filter the tests, use `cabal run test:tests -- -m "foo"`

## Troubleshooting
Seeing funny stuff when running lsp-test via stack? If your server is built upon Haskell tooling, [keep in mind that stack sets some environment variables related to GHC, and you may want to unset them.](https://github.com/alanz/haskell-ide-engine/blob/bfb16324d396da71000ef81d51acbebbdaa854ab/test/utils/TestUtils.hs#L290-L298)
