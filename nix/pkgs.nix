{ compiler ? "ghc8103" }:

let
  pkgs = import (
    builtins.fetchTarball {
      name   = "nixos-unstable-2021-01-10";
      url    = "https://github.com/NixOS/nixpkgs/archive/257cbbcd3ab.tar.gz";
      sha256 = "0g3n725kjk2fc9yn9rvdjwci4mrx58yrdgp3waby9ky3d5xhcaw4";
    }
  ) {};

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = newPkgs: oldPkgs: rec {
    };
  };
in
{
  pkgs = pkgs;
  hp = hp;
}
