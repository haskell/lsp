![CI](https://github.com/alanz/lsp/workflows/Haskell%20CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/lsp?label=lsp)](https://hackage.haskell.org/package/lsp)
[![Hackage](https://img.shields.io/hackage/v/lsp-types?label=lsp-types)](https://hackage.haskell.org/package/lsp-types)
[![Hackage](https://img.shields.io/hackage/v/lsp-test?label=lsp-test)](https://hackage.haskell.org/package/lsp-test)

# lsp
Haskell library for the Microsoft Language Server Protocol.
It currently implements all of the [3.15 specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/).

It is split into three separate packages, `lsp`, `lsp-types`, and `lsp-test`:
- `lsp-types` provides *type-safe* definitions that match up with the
typescript definitions laid out in the specification
- `lsp` is a library for building language servers, handling:
  - JSON-RPC transport
  - Keeping track of the document state in memory with the Virtual File System (VFS)
  - Responding to notifications and requests via handlers
  - Setting the server capabilities in the initialize request based on registered handlers
  - Dynamic registration of capabilities
  - Cancellable requests and progress notifications
  - Publishing and flushing of diagnostics
- `lsp-test` is a functional testing framework for Language Server Protocol servers.

## Language servers built on lsp
- [haskell-language-server](https://github.com/haskell/haskell-language-server)
- [dhall-lsp-server](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-lsp-server#readme)
- [swarm](https://github.com/byorgey/swarm/blob/main/src/Swarm/Language/LSP.hs)
- [curry-language-server](https://github.com/fwcd/curry-language-server)

## Example language servers
There are two example language servers in the `lsp/example/` folder. `Simple.hs` provides a minimal example:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MessageType_Info
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

            _ <- registerCapability mempty SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ InL rsp
            pure ()
          Right _ ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure ()
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp)
    ]

main :: IO Int
main =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      }
```

Whilst `Reactor.hs` shows how a reactor design can be used to handle all
requests on a separate thread, such in a way that we could then execute them on
multiple threads without blocking server communication. They can be installed
from source with

    cabal install lsp-demo-simple-server lsp-demo-reactor-server
    stack install :lsp-demo-simple-server :lsp-demo-reactor-server --flag haskell-lsp:demo
    
## Examples of using lsp-test

### Setting up a session

```haskell
import Language.LSP.Test
main = runSession "hie" fullCaps "proj/dir" $ do
  doc <- openDoc "Foo.hs" "haskell"
  skipMany anyNotification
  symbols <- getDocumentSymbols doc
```

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

Try out the example tests in the `lsp-test/example` directory with `cabal test`.
For more examples check the [Wiki](https://github.com/bubba/lsp-test/wiki/Introduction), or see this [introductory blog post](https://lukelau.me/haskell/posts/lsp-test/).

Whilst writing your tests you may want to debug them to see what's going wrong.
You can set the `logMessages` and `logStdErr` options in `SessionConfig` to see what the server is up to.
There are also corresponding environment variables so you can turn them on from the command line:
```
LSP_TEST_LOG_MESSAGES=1 LSP_TEST_LOG_STDERR=1 cabal test
```

## Troubleshooting
Seeing funny stuff when running lsp-test via stack? If your server is built upon Haskell tooling, [keep in mind that stack sets some environment variables related to GHC, and you may want to unset them.](https://github.com/alanz/haskell-ide-engine/blob/bfb16324d396da71000ef81d51acbebbdaa854ab/test/utils/TestUtils.hs#L290-L298)

## Useful links

- [Language Server Protocol Specification](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md)

## Other resources

See #haskell-language-server  on IRC freenode.

