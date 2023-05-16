# lsp-client

lsp-client is a Language Server Protocol client.

Notably, it does not use `conduit` to parse incoming messages, but
instead exposes a truly concurrent session, in which messages can be
sent and received asynchronously.

It is part of the [lsp](https://github.com/haskell/lsp) family of
packages, along with [lsp](https://hackage.haskell.org/package/lsp)
and [lsp-types](https://hackage.haskell.org/package/lsp-types).
