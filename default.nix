{ mkDerivation, base, stdenv, relude, mtl, text, optparse-applicative, turtle
, yaml, aeson, microlens, ansi-terminal ,  hasql, hasql-migration
, hasql-transaction, parsec, hspec, containers
}:
mkDerivation {
  pname = "migratum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base relude mtl text optparse-applicative turtle aeson yaml
      microlens ansi-terminal hasql hasql-migration hasql-transaction
      parsec hspec containers
    ];
  executableHaskellDepends = [ base relude ];
  license = "apache";
  hydraPlatforms = stdenv.lib.platforms.none;
}
