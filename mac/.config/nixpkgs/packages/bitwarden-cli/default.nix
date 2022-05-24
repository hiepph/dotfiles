{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  version = "1.22.1";

  buildInputs = [
    pkgs.unzip
  ];

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];

  src = pkgs.fetchurl {
    url = "https://github.com/bitwarden/cli/releases/download/v1.22.1/bw-macos-${version}.zip";
    sha256 = "63164D5FDA34F26AAEE34DADF6AE0D2B49D4C793F86ECACF200585FC92E742C4";
  };

  unpackPhase = ''
unzip $src
'';

  installPhase = ''
mkdir -p $out/bin
chmod +x ./bw
mv ./bw $out/bin
'';

  meta = with pkgs.lib; {
    description = "Bitwarden - Password manager";
    homepage = "https://bitwarden.com/";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
