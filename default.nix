{ mkDerivation, base, stdenv, relude, mtl, text, optparse-applicative, turtle
, yaml, aeson, ansi-terminal, hasql, cryptonite , hasql-transaction , parsec
, hspec, containers, vector, filepath
}:
mkDerivation {
  pname = "migratum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base relude mtl text optparse-applicative turtle aeson yaml
      ansi-terminal hasql hasql-transaction
      parsec hspec containers cryptonite vector filepath
    ];
  executableHaskellDepends = [ base relude ];
  license = "apache";
  hydraPlatforms = stdenv.lib.platforms.none;
}
