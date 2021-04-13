{
  description = "Package build and dev environment for Myopia";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  outputs = { self, haskellNix, nixpkgs }: {
    # setup derivation for x86_64-linux
    defaultPackage.x86_64-linux =
      let hn = haskellNix.legacyPackages.x86_64-linux.haskell-nix;
          drv = hn.project {
            src = hn.haskellLib.cleanGit {
              name = "Myopia";
              src = ./.;
            };
            compiler-nix-name = "ghc8104";
          };
      in drv.DraftGen;
   
    # setup devShell for x86_64-linux.
    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        inherit (lib) makeLibraryPath;
        hs = haskell.packages.ghc8104;
        tools = [
          binutils-unwrapped
          hlint
          hs.ghc
          hs.cabal-install
          hs.ghcid
          hs.fourmolu
        ];
        libraries = [
          pkg-config
          SDL2
          SDL2_gfx
          SDL2_image
        ];
        libraryPath = "${makeLibraryPath libraries}";
      in
        mkShell {
          buildInputs = tools ++ libraries;
          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
            export LIBRARY_PATH="${libraryPath}"
          '';
        };
  };
}
