{ mkDerivation, base, containers, parsec, process, stdenv }:
mkDerivation {
  pname = "kdmswitch";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parsec process ];
  license = stdenv.lib.licenses.bsd3;
}
