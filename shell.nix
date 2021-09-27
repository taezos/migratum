{ nixpkgs ? import ./nix/pinned.nix {} }:
let
  inherit ( nixpkgs ) pkgs;
  inherit ( pkgs ) haskellPackages;
in
pkgs.mkShell {
  name = "migratum";
  buildInputs = [
    haskellPackages.cabal-install
    haskellPackages.ghc
    haskellPackages.ghcid
    haskellPackages.hspec-discover
    pkgs.postgresql
    pkgs.zlib
    pkgs.hpack
  ];
}
