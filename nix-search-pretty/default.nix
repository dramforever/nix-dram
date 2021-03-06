{ mkDerivation, aeson, base, bytestring, lib, prettyprinter
, prettyprinter-ansi-terminal, text, unordered-containers, vector
}:
mkDerivation {
  pname = "nix-search-pretty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring prettyprinter prettyprinter-ansi-terminal
    text unordered-containers vector
  ];
  license = lib.licenses.bsd3;
}
