{ nixpkgs ? import ./nix/pinned.nix {} }:
let
  inherit ( nixpkgs ) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hasql-migration = doJailbreak(unmarkBroken super.hasql-migration);
    };
  };

  project = haskellPackages.callPackage ./default.nix {};

in
pkgs.stdenv.mkDerivation {
  name = "migratum";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghc
    haskellPackages.ghcid
    haskellPackages.hspec-discover
    pkgs.postgresql
    pkgs.zlib
  ];
}
