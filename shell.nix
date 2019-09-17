let
  pkgs = import <nixpkgs> {};

  addDeps = drv: pkgs.lib.overrideDerivation drv (attrs: {
    propagatedBuildInputs = attrs.propagatedBuildInputs ++ [pkgs.graphviz pkgs.pandoc];
  });

  drv    = addDeps (pkgs.haskellPackages.callPackage ./default.nix {});
  drvEnv = addDeps drv.env;

in
  if pkgs.lib.inNixShell then drvEnv else drv
