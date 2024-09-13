{
  description = "Doma fork of passveil";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {

          # For `nix develop`:
          devShell.tools = hp: {
            inherit (pkgs)
              pkg-config
              darcs

              # lint
              hlint

              # docs
              pandoc;
          };

        };
        # For `nix build` & `nix run`:
        packages.default = self'.packages.passveil;
      };
    };
}
