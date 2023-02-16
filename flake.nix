{
  description = "Package build and dev environment for Myopia";

  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix Package set
    nixpkgs.url = "github:nixos/nixpkgs?rev=d7705c01ef0a39c8ef532d1033bace8845a07d35";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux x86_64-darwin aarch64-darwin]) (system: let
      compiler-version = "ghc944";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # setup devShell.
      devShells.default = let
        inherit (pkgs.lib) makeLibraryPath;
        hs = pkgs.haskell.packages.${compiler-version};
        hlib = pkgs.haskell.lib;
        tools = [
          pkgs.binutils-unwrapped
          pkgs.hlint
          hs.ghc
          hs.cabal-install
          hs.cabal-plan
          hs.cabal-fmt
          hs.implicit-hie
          (hlib.dontCheck hs.ghcid)
          hs.fourmolu
          hs.haskell-language-server
        ];
        libraries = with pkgs; [pkg-config SDL2 SDL2_gfx SDL2_image zlib];
        libraryPath = "${makeLibraryPath libraries}";
      in
        pkgs.mkShell {
          name = "dev shell";
          buildInputs = tools ++ libraries;
          LD_LIBRARY_PATH = "${libraryPath}";
          LIBRARY_PATH = "${libraryPath}";
        };
      formatter = pkgs.alejandra;
    });
}
