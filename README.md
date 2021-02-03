![CI](https://github.com/alanz/lsp/workflows/Haskell%20CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/lsp?label=lsp)](https://hackage.haskell.org/package/lsp)
[![Hackage](https://img.shields.io/hackage/v/lsp?label=lsp-types)](https://hackage.haskell.org/package/lsp-types)

# lsp
Haskell library for the Microsoft Language Server Protocol.
It currently implements all of the [3.15 specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/).

It is split into two separate packages, `lsp` and `lsp-types`
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

## Language servers built on lsp
- [ghcide](https://github.com/haskell/ghcide)
- [haskell-language-server](https://github.com/haskell/haskell-language-server)
- [dhall-lsp-server](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-lsp-server#readme)

## Example language servers
There are two example language servers in the `example/` folder. `Simple.hs` provides a minimal example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageRequestParams MtInfo "Turn on code lenses?"
            (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      _ <- sendRequest SWindowShowMessageRequest params $ \res ->
        case res of
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)
              
            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
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

## Useful links

- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

## Other resources

See #haskell-ide-engine on IRC freenode

