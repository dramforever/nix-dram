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
        };
    }) // {
      overlay = import ./overlay.nix;
    };
}
