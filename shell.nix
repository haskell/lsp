{ withHoogle ? false, nixpkgs ? import ./nix { } }:
with nixpkgs;
with ourHaskellPackages;
shellFor {
  inherit withHoogle;
  doBenchmark = true;
  doTest = true;
  packages = p: with builtins; map (name: p.${name}) (attrNames ourSources);
  buildInputs = [ cabal-install nixpkgs.haskellPackages.haskell-language-server nixpkgs.haskellPackages.fourmolu ];
}
