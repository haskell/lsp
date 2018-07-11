# haskell-lsp-test [![Build Status](https://travis-ci.com/Bubba/haskell-lsp-test.svg?branch=master)](https://travis-ci.com/Bubba/haskell-lsp-test)
This is the functional testing framework for [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine), and potentially any other Language Server Protocol server.

```haskell
runSession "session/root/dir" $ do
  doc <- openDoc "foo.hs" "haskell"
  
  skipMany notification

  symbols <- getDocumentSymbols doc
```

## Developing
To test make sure you have [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) installed.
