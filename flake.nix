{
  description = "Nix with a modified frontend, by dramforever";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  nixConfig = {
    substituters = [ "https://dram.cachix.org" ];
    trusted-public-keys = [ "dram.cachix.org-1:baoy1SXpwYdKbqdTbfKGTKauDDeDlHhUpC+QuuILEMY=" ];
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      checks = packages;
      defaultPackage = packages.nix-dram;

      defaultApp = {
        type = "app";
        program = "${packages.nix-dram}/bin/nix";
      };
      packages =
        let pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
        in {
          inherit (pkgs) nix-search-pretty nix-nar-listing nix-dram;
        };
    }) // {
      overlay = final: prev: {
        nix-search-pretty =
          final.haskellPackages.callPackage ./nix-search-pretty {};

        nix-nar-listing =
          final.haskellPackages.callPackage ./nix-nar-listing {};

        nix-dram = final.nixUnstable.overrideAttrs (old: {
          name = "nix-dram-" + old.version;
          patches = (old.patches or []) ++ [
            ./nix-patches/nix-flake-default.patch
            ./nix-patches/nix-search-meta.patch
          ];
        });
      };
    };
}
