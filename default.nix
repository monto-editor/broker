{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; cabal.mkDerivation (self: {
  pname = "monto-broker";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    cabalInstall zeromq4Haskell aeson hspec QuickCheck fgl optparseApplicative
    lattices pkgs.docker pkgs.graphviz
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
