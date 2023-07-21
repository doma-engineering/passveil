{
  description = "Passveil flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  # Revision of nixpkgs with right version of dependencies
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/5c1ffb7a9fc96f2d64ed3523c2bdd379bdb7b471";
  inputs.unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs, unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs_unstable = unstable.legacyPackages.${system};
      in
      {
        packages = rec {
          passveil = pkgs.haskellPackages.callPackage ./passveil.nix { fetchzip = pkgs_unstable.fetchzip; };
          default = passveil;
        };
      }
    );
}
