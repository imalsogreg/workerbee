{ mkDerivation, aeson, algebraic-graphs, base, bytestring
, containers, dhall, hspec, http-api-data, http-media, HUnit, mtl
, pandoc, process, servant, servant-client, servant-docs
, servant-server, stdenv, stm, temporary, text
, unordered-containers, warp
}:
mkDerivation {
  pname = "workerbee";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson algebraic-graphs base bytestring containers dhall hspec
    http-api-data http-media HUnit mtl pandoc process servant
    servant-client servant-docs servant-server stm temporary text
    unordered-containers warp
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
