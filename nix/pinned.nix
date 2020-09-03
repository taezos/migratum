let
  pkgs = import <nixpkgs> {};
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "c643d583498157128e5722898265ab81f76b0a6e";
    sha256 = "1c8ip0y16fzxr5pybyf1x9flzb3z9x5ap6f8xkayd0xkgvj830x2";
  }
)
# https://github.com/NixOS/nixpkgs/archive/c643d583498157128e5722898265ab81f76b0a6e.tar.gz
