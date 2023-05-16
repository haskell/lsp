{ sources ? import ./sources.nix }:

let
  overlay = self: super:
    let
      inherit (import sources."gitignore.nix" { inherit (super) lib; })
        gitignoreSource;
      ourSources = {
        lsp = gitignoreSource ../lsp;
        lsp-client = gitignoreSource ../lsp-client;
        lsp-test = gitignoreSource ../lsp-test;
        lsp-types = gitignoreSource ../lsp-types;
      };

    in {
      inherit gitignoreSource;
      inherit ourSources;
      ourHaskellPackages = with super.haskell.lib;
        super.haskellPackages.extend (packageSourceOverrides ourSources);
    };
in (import sources.nixpkgs {
  overlays = [ overlay ];
  config = { allowBroken = true; };
})
