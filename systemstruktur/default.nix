{ mkDerivation, base, stdenv, containers, QuickCheck }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers QuickCheck ];
  homepage = "https://github.com/mikesperber/funar/";
  license = stdenv.lib.licenses.bsd3;
}
