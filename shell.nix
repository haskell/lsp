{ nixpkgs ? import ./nix { } }:
with nixpkgs;
let 
  hsPkgs = haskell.packages.ghc946;
  ghc = haskell.compiler.ghc946;
in mkShell {
  nativeBuildInputs = [ 
    hsPkgs.ghc 
    hsPkgs.haskell-language-server 
    cabal-install 
    zlib 
    pkg-config ];
}
