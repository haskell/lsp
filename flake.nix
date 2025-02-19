{
  description = "Haskell development environment";

  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      flake-utils,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            # Haskell toolchain.
            hpkgs.cabal-fmt
            hpkgs.cabal-install
            hpkgs.fourmolu
            hpkgs.ghc
            hpkgs.haskell-language-server

            # Misc.
            pkgs.zlib
            pkgs.pkg-config
          ];
        };
      }
    );
}
