{ mkDerivation, base, stdenv, containers, parallel, QuickCheck }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parallel QuickCheck ];
  homepage = "https://github.com/mikesperber/funar/";
  license = stdenv.lib.licenses.bsd3;
}
