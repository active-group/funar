{ mkDerivation, base, stdenv, containers, parallel, QuickCheck, sqlite-simple }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parallel QuickCheck sqlite-simple ];
  homepage = "https://github.com/mikesperber/funar/";
  license = stdenv.lib.licenses.bsd3;
}
