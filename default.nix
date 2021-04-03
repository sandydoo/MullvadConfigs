{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, http-conduit, iproute, lens, lens-aeson, lib, relude
, text, zip
}:
mkDerivation {
  pname = "MullvadConfigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath http-conduit
    iproute lens lens-aeson relude text zip
  ];
  executableHaskellDepends = [
    aeson base bytestring containers directory filepath http-conduit
    iproute lens lens-aeson relude text zip
  ];
  homepage = "https://github.com/sandydoo/MullvadConfigs#readme";
  license = lib.licenses.bsd3;
}
