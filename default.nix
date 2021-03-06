{ mkDerivation, aeson, aeson-casing, base, bytestring, containers, fgl, hspec
, mtl, pretty-show, optparse-applicative, QuickCheck, semigroups, stdenv, text
, unix, unordered-containers, vector, zeromq4-haskell
}:
mkDerivation {
  pname = "monto-broker";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring containers fgl semigroups text
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring containers fgl optparse-applicative text unix
    vector zeromq4-haskell
  ];
  testHaskellDepends = [
    base bytestring containers fgl hspec mtl QuickCheck text vector
  ];
  doCheck = false;
  description = "Haskell Broker for Monto that implements Service Dependencies";
  license = stdenv.lib.licenses.bsd3;
}
