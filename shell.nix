{ pkgs ? import <nixpkgs> {} }:

let env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install zeromq4-haskell hspec QuickCheck fgl optparse-applicative
    aeson vector hlint
  ]);

in pkgs.stdenv.mkDerivation {
  name = "monto-broker";
  version = "0.0.1";
  src = ./.;
  buildInputs = [ env ];
}
