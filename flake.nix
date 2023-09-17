{
  description = "Package build and dev environment for Myopia";

  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix Package set
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [
      x86_64-linux
      x86_64-darwin
      aarch64-linux
      aarch64-darwin
    ]) (system: let
      compiler-version = "ghc946";
      # Be explicit about which overlays are in use
      overlays = {alejandra = import ./nix/overlays/alejandra/default.nix;};
      inherit (nixpkgs) lib;
      pkgs =
        builtins.foldl' (acc: overlay: acc.extend overlay)
        nixpkgs.legacyPackages.${system} (builtins.attrValues overlays);
      # Nix formatter
      formatter = pkgs.alejandra;
    in {
      apps.check-formatting = {
        type = "app";
        program = let
          name = "nix-check-formatting";
          script =
            pkgs.writeShellScriptBin name
            "${formatter}/bin/alejandra --check * --exclude dist-newstyle";
        in "${script}/bin/${name}";
      };
      devShells.default = let
        hs = pkgs.haskell.packages.${compiler-version};
        hlib = pkgs.haskell.lib;
        tools = [
          pkgs.binutils-unwrapped
          pkgs.hlint
          hs.ghc
          hs.cabal-install
          # hs.cabal-plan
          hs.cabal-fmt
          hs.implicit-hie
          (hlib.dontCheck hs.ghcid)
          hs.fourmolu
          hs.haskell-language-server
        ];
        libraries = with pkgs; [freetype glib harfbuzz libwebp libtiff pcre2 pkg-config SDL2 SDL2_gfx SDL2_image SDL2_ttf zlib];
        libraryPath = with pkgs.lib; "${makeLibraryPath libraries}";
      in
        pkgs.mkShell {
          name = "dev shell";
          buildInputs = tools ++ libraries;
          LD_LIBRARY_PATH = "${libraryPath}";
          LIBRARY_PATH = "${libraryPath}";
        };
      inherit formatter;
    });
}
