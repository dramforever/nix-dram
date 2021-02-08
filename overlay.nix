final: prev: {
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
}
