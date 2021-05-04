# Myopia
[![simple haskell ](https://img.shields.io/badge/Simple-Haskell-purple?style=for-the-badge)](https://www.simplehaskell.org/) [![license](https://img.shields.io/github/license/skykanin/Myopia?color=%23BD0000&style=for-the-badge)](https://www.gnu.org/licenses/gpl-3.0.en.html)

A simple Roguelike game built using SDL and Haskell.

# Documentation
TODO

# Develop
## Prerequisites
If you want to use the development environment includeded in this repository you
will need to install [nix](https://nixos.org/download.html#nix-quick-install) and
enable experimental features by running:
- `nix-env -iA nixpkgs.nixFlakes`
- `mkdir ~/.config/nix && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf`

If you don't want to use nix for the development you will have to provide the system dependencies yourself.
These can be found listed in the `flake.nix` file under `libraries`.
The required system dependencies are:
- `pkg-config`
- `SDL2`
- `SDL2_gfx`
- `SDL2_image`

## Start contributing!
Run `nix develop` to enter the development environment.

This adds everything listed in the `flake.nix` files `devShell` attribute to your path. It is also possible to automatically hook into this environment using `direnv` see instructions on how to set this up [here](https://github.com/direnv/direnv/wiki/Nix).

## Run
On the first run you will be prompted to run `cabal update`.

After that you can run the project with `cabal run`. To pass command line arguments to cabal you run `cabal run <artifact> -- <args go here>`.

## Build
To build the project using cabal you can run
`cabal build [...opts]`
