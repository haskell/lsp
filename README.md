# haskell-lsp
Haskell library for the Microsoft Language Server Protocol

## Hacking

First, `cabal install` this project so that the `hie-vscode` executable is in
the path.

Then, run the plugin in vscode:

    git clone https://github.com/alanz/vscode-hie-server
    cd vscode-hie-server
    code .

In vscode, press F5 to run the extension in development mode.

You can see a log from `hie-vscode` by doing

    tail -F /tmp/hie-vscode.log 

There are also facilities on the code to send back language-server-protocol log
and show events.


## Useful links

- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md
- https://github.com/gorkem/language-server-protocol/blob/e54acb19c157b6248863a20e6ceb968eb2dc9caa/protocol.json
- https://github.com/Microsoft/vscode-languageserver-node/blob/master/server/src/main.ts
- https://github.com/Microsoft/vscode-languageserver-node/blob/master/jsonrpc/src/main.ts
- https://hackage.haskell.org/package/phoityne-vscode
