{ mkDerivation
, aeson
, ansi-terminal
, base
, base16-bytestring
, bytestring
, containers
, cryptohash-sha256
, directory
, filepath
, hashable
, hexstring
, lib
, megaparsec
, mtl
, optparse-applicative
, process
, random
, regex-base
, regex-pcre
, temporary
, text
, time
, unix
, unordered-containers
, fetchzip
, darcs
, makeWrapper
}:

mkDerivation {
  pname = "passveil";
  version = "0.1.0.0";
  src = fetchzip {
    url = "https://hub.darcs.net/antei/passveil/dist";
    sha256 = "sha256-0Hee/snKz09T5QSrL+n8gkzYFN+0JWHE6vfR9N2wTeQ=";
    extension = "zip";
  };

  patches = [ ./passveil-cabal.patch ];

  isLibrary = true;
  isExecutable = true;
  jailbreak = true;
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    cryptohash-sha256
    directory
    filepath
    hashable
    hexstring
    megaparsec
    mtl
    process
    random
    text
    time
    unordered-containers
  ];
  executableHaskellDepends = [
    hexstring
    aeson
    ansi-terminal
    base
    bytestring
    containers
    directory
    filepath
    mtl
    optparse-applicative
    process
    regex-base
    regex-pcre
    temporary
    text
    time
    unix
    unordered-containers
    darcs
  ];

  buildTools = [ makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/passveil \
      --prefix PATH : ${lib.getBin darcs}/bin
  '';

  extraLibraries = [ darcs ];

  license = "unknown";
  #  mainProgram = "passveil";
}
