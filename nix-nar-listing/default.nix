{ mkDerivation, aeson, base, bytestring, lib, prettyprinter
, prettyprinter-ansi-terminal, text, unordered-containers
}:
mkDerivation {
  pname = "nix-nar-listing";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring prettyprinter prettyprinter-ansi-terminal
    text unordered-containers
  ];
  executableHaskellDepends = [ base bytestring text ];
  license = lib.licenses.bsd3;
}
