# lsp

lsp is a library for building language servers, handling:

- JSON-RPC transport
- Keeping track of the document state in memory with the Virtual File System (VFS)
- Responding to notifications and requests via handlers
- Setting the server capabilities in the initialize request based on registered handlers
- Dynamic registration of capabilities
- Cancellable requests and progress notifications
- Publishing and flushing of diagnostics

It is part of the [lsp](https://github.com/haskell/lsp) family of packages,
along with [lsp-test](https://hackage.haskell.org/package/lsp-test) and
[lsp-types](https://hackage.haskell.org/package/lsp-types)

For more information, check https://github.com/haskell/lsp/blob/master/README.md
