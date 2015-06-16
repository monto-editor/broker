{ pkgs ? import <nixpkgs> {} }:

let pkg = pkgs.haskellPackages.callPackage (
  { mkDerivation, base, ghc,cabal-install,zeromq4-haskell,aeson,hspec,QuickCheck,fgl, optparse-applicative }:
  mkDerivation {
    pname = "monto-broker";
    version = "0.0.1";
    src = ./.;
    buildDepends = [
      ghc cabal-install zeromq4-haskell aeson hspec QuickCheck fgl optparse-applicative
    ];
    license = pkgs.stdenv.lib.licenses.bsd3;
  }) {};
in pkg.env
