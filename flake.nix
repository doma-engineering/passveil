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
      perSystem = { pkgs, ... }:
      let
        hp = pkgs.haskellPackages;

        # Define the base passveil package
        passveil = hp.callCabal2nix "passveil" ./. {};

        # Override attributes to add postInstall and nativeBuildInputs
        passveilWithCompletions = passveil.overrideAttrs (oldAttrs: {
          postInstall = ''
            ${oldAttrs.postInstall or ""}
            install -Dm644 contrib/bash/completions/passveil.bash "$out/share/bash-completion/completions/passveil"
            install -Dm644 contrib/zsh/completions/_passveil "$out/share/zsh/site-functions/_passveil"
            install -Dm644 contrib/fish/completions/passveil.fish "$out/share/fish/vendor_completions.d/passveil.fish"
          '';
        });
      in {
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
        packages.default = passveilWithCompletions;
      };
    };
}
