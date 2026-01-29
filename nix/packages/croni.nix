{
  buildDunePackage,
  lib,
  angstrom,
  ptime,
  alcotest,
  doCheck ? true,
}:

buildDunePackage {
  pname = "croni";
  version = "0.1.0";

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../lib
        ../../dune-project
        ../../croni.opam
      ];
    };

  propagatedBuildInputs = [
    angstrom
    ptime
  ];
  inherit doCheck;

  checkInputs = [ alcotest ];
}
