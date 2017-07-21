[![CircleCI](https://img.shields.io/circleci/project/github/alanz/haskell-lsp.svg)]()
[![Hackage](https://img.shields.io/hackage/v/haskell-lsp.svg)](https://hackage.haskell.org/package/haskell-lsp)

# haskell-lsp
Haskell library for the Microsoft Language Server Protocol

Warning: this library and its associated ecosystem is under development at the
moment. So do not have high expectations, it is not ready for casual use.

## Hacking

To see this library in use you need to install the [haskell-ide-engine](https://github.com/alanz/haskell-ide-engine/)

    git clone https://github.com/alanz/haskell-ide-engine
    cd haskell-ide-engine
    stack install

This will put the `hie` executable in your path.

Then, run the plugin in vscode:

    git clone https://github.com/alanz/vscode-hie-server
    cd vscode-hie-server
    code .

In vscode, press F5 to run the extension in development mode.

You can see a log from `hie` by doing

    tail -F /tmp/hie-vscode.log

There are also facilities on the code to send back language-server-protocol log
and show events.

It can also be used with emacs, see https://github.com/emacs-lsp/lsp-haskell

## Using the example server

    stack install

will generate a `lsp-hello` executable.

Changing the server to be called in the [`vscode-hie-server`](https://github.com/alanz/vscode-hie-server/blob/master/hie-vscode.sh#L21) plugin from `hie` to
`lsp-hello` will run the example server instead of hie.

Likewise, changing the executable in `lsp-haskell` for emacs.

## Useful links

- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

## Other resource

See #haskell-ide-engine on IRC freenode

