{
  description = "Nix with a modified frontend, by dramforever";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  nixConfig = {
    extra-substituters = [ "https://dram.cachix.org" ];
    extra-trusted-public-keys = [ "dram.cachix.org-1:baoy1SXpwYdKbqdTbfKGTKauDDeDlHhUpC+QuuILEMY=" ];
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
        checks = {
          inherit (packages)
            # nix-dram-progress # broken
            nix-dram
            nix-search nix-search-pretty
            nix-nar-listing;
        };

        defaultPackage = packages.nix-dram;

        defaultApp = apps.nix-dram;

        apps = {
          nix-dram = {
            type = "app";
            program = "${packages.nix-dram}/bin/nix";
          };

          default = apps.nix-dram;
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

            default = packages.nix-dram;
          };
      }) // {
      overlay = final: prev: {
        nix-search-pretty =
          final.haskellPackages.callPackage ./nix-search-pretty {};

        nix-nar-listing =
          final.haskellPackages.callPackage ./nix-nar-listing {};

        make-nix-dram = { nix }:
          (nix.appendPatches [
            ./nix-patches/nix-flake-default.patch
            ./nix-patches/nix-search-meta.patch
            ./nix-patches/nix-environment.patch
            ./nix-patches/nix-git-fetch-no-quiet.patch
          ]).overrideAttrs (old: {
            name = "nix-dram-" + old.version;
          });

        nix-dram = final.make-nix-dram {
          nix = final.nixVersions.latest;
        };

        nix-dram-progress = final.callPackage ({ nix }: nix.overrideAttrs (old: {
            name = "nix-dram-" + old.version;
          patches = (old.patches or []) ++ [
              ./nix-patches/nix-flake-default.patch
              ./nix-patches/nix-search-meta.patch
              ./nix-patches/nix-environment.patch
              ./nix-patches/nix-git-fetch-no-quiet.patch
              (final.fetchpatch {
                name = "nix-progress.diff";
                url = "https://github.com/NixOS/nix/compare/480426a...1af0a16.diff";
                sha256 = "sha256-vR7kGQMLHcf2qnaycyrv8h9M5iZjIC+GxD9kfqM3lzQ=";
              })
            ];
        })) {
            nix = final.nixVersions.latest;
          };

        nix-search = final.writeShellScriptBin "nix-search" ''
          ${final.nix-dram}/bin/nix search --json "$@" | ${final.nix-search-pretty}/bin/nix-search-pretty
        '';
      };
    };
}
