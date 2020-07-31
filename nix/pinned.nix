let
  pkgs = import <nixpkgs> {};
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "ccd458053b0eaf66c1f639ee1396248d8f4431e2";
    sha256 = "1qwnmmb2p7mj1h1ffz1wvkr1v55qbhzvxr79i3a15blq622r4al9";
  }
)
# https://github.com/NixOS/nixpkgs/archive/ccd458053b0eaf66c1f639ee1396248d8f4431e2.tar.gz
