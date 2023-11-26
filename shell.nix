{ nixpkgs ? import ./nix { } }:
with nixpkgs;
let 
  hsPkgs = haskell.packages.ghc946;
  ghc = haskell.compiler.ghc946;
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
