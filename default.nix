{ mkDerivation, base, stdenv, relude, mtl, text, optparse-applicative, turtle
, filepath, yaml, aeson, microlens-platform, ansi-terminal, co-log, co-log-core
}:
mkDerivation {
  pname = "migratum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base relude mtl text optparse-applicative turtle filepath aeson yaml
      microlens-platform ansi-terminal co-log co-log-core
    ];
  executableHaskellDepends = [ base relude ];
  license = "apache";
  hydraPlatforms = stdenv.lib.platforms.asl20;
}
