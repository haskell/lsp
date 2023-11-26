{ nixpkgs ? import ./nix { } }:
with nixpkgs;
let 
  hsPkgs = haskellPackages;
  ghc = haskellPackages.ghc;
in mkShell {
  nativeBuildInputs = [ 
    hsPkgs.ghc 
    hsPkgs.haskell-language-server 
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    cabal-install 
    zlib 
    pkg-config ];
}
