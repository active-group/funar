{ mkDerivation, base, stdenv, containers, parallel, QuickCheck, text, sqlite-simple }:
mkDerivation {
  pname = "funar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parallel QuickCheck text sqlite-simple];
  license = stdenv.lib.licenses.unfree;
}
