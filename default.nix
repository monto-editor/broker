{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; cabal.mkDerivation (self: {
  pname = "monto-broker";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    cabalInstall zeromq4Haskell aeson
    hspec QuickCheck fgl pkgs.graphviz
    optparseApplicative libsystemdJournal
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
