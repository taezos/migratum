{ nixpkgs ? import ./nix/pinned.nix {} }:
let
  inherit ( nixpkgs ) pkgs;
  inherit ( pkgs ) haskellPackages;

  project = haskellPackages.callPackage ./default.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "migratum";
  buildInputs = project.env.buildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghc
    haskellPackages.ghcid
    haskellPackages.hspec-discover
    pkgs.postgresql
    pkgs.zlib
  ];
}
