{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, http-conduit, lens, lens-aeson, lib, relude, text
, text-builder, zip
}:
mkDerivation {
  pname = "MullvadConfigs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath http-conduit
    lens lens-aeson relude text text-builder zip
  ];
  executableHaskellDepends = [
    aeson base bytestring containers directory filepath http-conduit
    lens lens-aeson relude text text-builder zip
  ];
  homepage = "https://github.com/sandydoo/MullvadConfigs#readme";
  license = lib.licenses.bsd3;
}
