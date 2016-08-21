{ mkDerivation, base, cmdargs, containers, raw-strings-qq, stdenv
, trifecta
}:
mkDerivation {
  pname = "nrrdchecker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cmdargs containers raw-strings-qq trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
