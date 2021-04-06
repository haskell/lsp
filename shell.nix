{ withHoogle ? false, nixpkgs ? import ./nix { } }:
with nixpkgs;
with ourHaskellPackages;
shellFor {
  inherit withHoogle;
  doBenchmark = true;
  packages = p: with builtins; map (name: p.${name}) (attrNames ourSources);
  buildInputs = [ cabal-install ];
}
