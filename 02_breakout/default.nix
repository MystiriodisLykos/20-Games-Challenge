let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  pong = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
pong
