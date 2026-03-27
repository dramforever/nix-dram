{
  description = "Nix with a modified frontend, by dramforever";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  nixConfig = {
    extra-substituters = [ "https://dram.cachix.org" ];
    extra-trusted-public-keys = [ "dram.cachix.org-1:baoy1SXpwYdKbqdTbfKGTKauDDeDlHhUpC+QuuILEMY=" ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      checks = {
        inherit (packages)
          nix-dram
          nix-search
          nix-search-pretty
          nix-nar-listing
          ;
      };

      packages =
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlay ];
          };
        in
        {
          inherit (pkgs)
            nix-dram
            nix-search
            nix-search-pretty
            nix-nar-listing
            ;

          default = packages.nix-dram;
        };
    })
    // {
      overlay = final: prev: {
        nix-search-pretty = final.haskellPackages.callPackage ./nix-search-pretty { };

        nix-nar-listing = final.haskellPackages.callPackage ./nix-nar-listing { };

        make-nix-dram =
          { nix }:
          (nix.appendPatches [
            ./nix-patches/nix-flake-default.patch
            ./nix-patches/nix-search-meta.patch
            ./nix-patches/nix-environment.patch
          ]).overrideAttrs
            (old: {
              name = "nix-dram-" + old.version;
            });

        nix-dram = final.make-nix-dram {
          nix = final.nixVersions.latest;
        };

        nix-search = final.writeShellScriptBin "nix-search" ''
          ${final.nix-dram}/bin/nix search --json "$@" | ${final.nix-search-pretty}/bin/nix-search-pretty
        '';
      };
    };
}
