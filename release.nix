let
  pkgs = import ./nix/pinned.nix {};

  # remove this when haskell-updates branch with hasql-migration marked as
  # unbroken is merged to master
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: rec {
      hasql-migration = self.callCabal2nix "hasql-migration" ( builtins.fetchGit {
        url = "git@github.com:tvh/hasql-migration.git";
        rev = "58c90f7c5c0e829708c35db9d2c8bc47e86621eb";
      }){};
    };
  };
in
  haskellPackages.callPackage ./default.nix {}
