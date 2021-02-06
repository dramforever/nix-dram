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

      defaultApp = apps.nix-dram;

      apps = {
        nix-dram = {
          type = "app";
          program = "${packages.nix-dram}/bin/nix";
        };

        nix-dram-progress = {
          type = "app";
          program = "${packages.nix-dram-progress}/bin/nix";
        };
      };

      packages =
        let pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
        in {
          inherit (pkgs)
            nix-dram nix-dram-progress
            nix-search nix-search-pretty
            nix-nar-listing;

          nix-dram-static = pkgs.pkgsStatic.nix-dram;
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

        nix-dram-progress = final.nixUnstable.overrideAttrs (old: {
          name = "nix-dram-" + old.version;
          patches = (old.patches or []) ++ [
            ./nix-patches/nix-flake-default.patch
            ./nix-patches/nix-search-meta.patch
            (final.fetchpatch {
              name = "add-notice-level.diff";
              url = "https://github.com/NixOS/nix/commit/a8f533b66417a1025a468cae3068bd2f5c06e811.patch";
              sha256 = "sha256-1LR6Vdxx0isClossrURuoNAGilISRfLoegGRJ7lJn2w=";
            })
            (final.fetchpatch {
              name = "nix-progress.diff";
              url = "https://github.com/NixOS/nix/compare/480426a...1af0a16.diff";
              sha256 = "sha256-vR7kGQMLHcf2qnaycyrv8h9M5iZjIC+GxD9kfqM3lzQ=";
            })
          ];
        });

        nix-search = final.writeShellScriptBin "nix-search" ''
          ${final.nix-dram}/bin/nix search --json "$@" | ${final.nix-search-pretty}/bin/nix-search-pretty
        '';
      };
    };
}
