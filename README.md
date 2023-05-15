# Myopia
[![license](https://img.shields.io/github/license/skykanin/Myopia?color=%23BD0000&style=for-the-badge)](https://www.gnu.org/licenses/gpl-3.0.en.html)

A simple Roguelike game built using SDL and Haskell.

# Documentation
TODO

# Develop
## Prerequisites
If you want to use the development environment includeded in this repository you
will need to install [nix](https://zero-to-nix.com/start/install). The install will also enable all necessary "experimental" nix features you need.

If you don't want to use nix for the development you will have to provide the system dependencies yourself.
These can be [found](https://github.com/skykanin/Myopia/blob/main/flake.nix#LL60C33-L60C48) listed in the `flake.nix` file under `libraries`.

## Start contributing!
Run `nix develop` to enter the development environment.

This adds everything listed in the `flake.nix` files `devShell` attribute to your path. It is also possible to automatically hook into this environment using `direnv` see instructions on how to set this up [here](https://github.com/direnv/direnv/wiki/Nix).

### Build
To build the project using `cabal` you can run
```
cabal build myopia
```

### Run
Run the project with
```
cabal run myopia
```

### Tests
To execute all unit tests run
```
cabal run test:unit-tests
```
