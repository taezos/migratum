let
  pkgs = import <nixpkgs> {};
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "f1bc04254b98c0e2b9c1f232a2be9b5a41f71efb";
    sha256 = "1ii3r8553xyv63a4w3rvlnj49vrh9m24kmwfvixbbswcp3n87x2a";
  }
)
# https://github.com/NixOS/nixpkgs/archive/f1bc04254b98c0e2b9c1f232a2be9b5a41f71efb.tar.gz
