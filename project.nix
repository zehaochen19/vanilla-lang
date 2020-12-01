{ mkDerivation, aeson, base, bytestring, containers, directory
, extra, filepath, hspec, megaparsec, mtl, parser-combinators
, stdenv, text
}:
mkDerivation {
  pname = "vanilla-lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory extra filepath
    megaparsec mtl parser-combinators text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers directory extra filepath
    megaparsec mtl parser-combinators text
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory extra filepath hspec
    megaparsec mtl parser-combinators text
  ];
  homepage = "https://github.com/zehaochen19/vanilla-lang#readme";
  license = stdenv.lib.licenses.agpl3;
}
