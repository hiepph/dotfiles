{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "nushell";
  version = "0.71.0";

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/bin
cp nu-${version}-x86_64-apple-darwin/nu $out/bin
'';
  src = pkgs.fetchurl {
    url = "https://github.com/nushell/nushell/releases/download/${version}/nu-${version}-x86_64-apple-darwin.tar.gz";
    sha256 = "03bxg2rk44y12sj9wig9kylmpzq4jxmsdmif5rsd1hy2fgm4phix";
  };

  meta = with pkgs.lib; {
    description = "nushell";
    homepage = "https://github.com/nushell/nushell";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
