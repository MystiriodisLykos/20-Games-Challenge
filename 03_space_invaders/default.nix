let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  space = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
space
