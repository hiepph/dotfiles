{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "ripgrep-mac";
  version = "13.0.0";

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/bin
cp ripgrep-13.0.0-x86_64-apple-darwin/rg $out/bin
'';
  src = pkgs.fetchurl {
    url = "https://github.com/BurntSushi/ripgrep/releases/download/${version}/ripgrep-${version}-x86_64-apple-darwin.tar.gz";
    sha256 = "10l024z44h6lgdymcfy0pxyakmgdwqhwkmpdc4j3km5q1hsihp2q";
  };

  meta = with pkgs.lib; {
    description = "Ripgrep";
    homepage = "https://github.com/BurntSushi/ripgrep";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
