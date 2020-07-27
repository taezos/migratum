let
  pkgs = import <nixpkgs> {};
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "6667f47c5bdd7939b6fbabf5831d2a00d453bbc4";
    sha256 = "1fc743z1aab9c6wx8ayk56isrs5jxra5hm027pz13jlnryida0qs";
  }
)
# https://github.com/NixOS/nixpkgs/archive/6667f47c5bdd7939b6fbabf5831d2a00d453bbc4.tar.gz
