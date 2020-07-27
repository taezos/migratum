{ mkDerivation, base, stdenv, relude, mtl, text, optparse-applicative, turtle
, yaml, aeson, microlens-th, microlens, ansi-terminal
, casing, hasql, hasql-migration, hasql-transaction, parsec, extra, hspec
}:
mkDerivation {
  pname = "migratum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base relude mtl text optparse-applicative turtle aeson yaml
      microlens-th microlens ansi-terminal casing hasql
      hasql-migration hasql-transaction parsec extra hspec
    ];
  executableHaskellDepends = [ base relude ];
  license = "apache";
  hydraPlatforms = stdenv.lib.platforms.none;
}
