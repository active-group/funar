{ mkDerivation, base, stdenv, containers, QuickCheck, text, sqlite-simple }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers QuickCheck text sqlite-simple ];
  homepage = "https://github.com/mikesperber/funar/";
  license = stdenv.lib.licenses.bsd3;
}
