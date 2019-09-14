{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, algebraic-graphs, base, bytestring
      , containers, dhall, hspec, HUnit, servant, servant-client
      , servant-docs, servant-server, stdenv, text, unordered-containers
      }:
      mkDerivation {
        pname = "workerbee";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson algebraic-graphs base bytestring containers dhall hspec HUnit
          servant servant-client servant-docs servant-server text
          unordered-containers
        ];
        executableHaskellDepends = [ base ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
