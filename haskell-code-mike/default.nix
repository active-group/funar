{ mkDerivation, base, stdenv, containers, parallel, QuickCheck, text, sqlite-simple }:
mkDerivation {
  pname = "funar-code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parallel QuickCheck text sqlite-simple ];
  homepage = "https://github.com/active-group/funar/";
  license = stdenv.lib.licenses.bsd3;
}
