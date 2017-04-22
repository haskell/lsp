# haskell-lsp
Haskell library for the Microsoft Language Server Protocol

Warning: this library and its associated ecosystem is under development at the
moment. So do not have high expectations, it is not ready for casual use.

## Hacking

To see this library in use you need to install the [haskell-ide-engine](https://github.com/alanz/haskell-ide-engine/tree/lsp)
(Note: this is not the master from the haskell repo, it is the work in progress one)

    git clone https://github.com/alanz/haskell-ide-engine
    cd haskell-ide-engine
    git checkout lsp
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

## Useful links

- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

## Other resource

See #haskell-ide-engine on IRC freenode

