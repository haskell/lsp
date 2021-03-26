# lsp-test

lsp-test is a functional testing framework for Language Server Protocol servers.

It is part of the [lsp](https://github.com/haskell/lsp) family of
packages, along with [lsp](https://hackage.haskell.org/package/lsp)
and [lsp-types](https://hackage.haskell.org/package/lsp-types)

```haskell
import Language.LSP.Test
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
For more examples see this [introductory blog post](https://lukelau.me/haskell/posts/lsp-test/).

Whilst writing your tests you may want to debug them to see what's going wrong.
You can set the `logMessages` and `logStdErr` options in `SessionConfig` to see what the server is up to.
There are also corresponding environment variables so you can turn them on from the command line:
```
LSP_TEST_LOG_MESSAGES=1 LSP_TEST_LOG_STDERR=1 cabal test
```

## Developing
The tests for lsp-test use a dummy server found in `test/dummy-server/`.
Run the tests with `cabal test` or `stack test`.
Tip: If you want to filter the tests, use `cabal run test:tests -- -m "foo"`

## Troubleshooting
Seeing funny stuff when running lsp-test via stack? If your server is built upon Haskell tooling, [keep in mind that stack sets some environment variables related to GHC, and you may want to unset them.](https://github.com/alanz/haskell-ide-engine/blob/bfb16324d396da71000ef81d51acbebbdaa854ab/test/utils/TestUtils.hs#L290-L298)
