{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "zig";
  version = "0.10.0";

  sourceRoot = ".";
  distName = "zig-macos-x86_64-0.10.0-dev.1261+6f986298c";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/bin
cp $distName/zig $out/bin
cp -r $distName/lib $out/bin
'';
  src = pkgs.fetchurl {
    url = "https://ziglang.org/builds/${distName}.tar.xz";
    sha256 = "5e7ab6816ae8439bc36fe7a07adc130112d1e55285f9259d6cd1a965cccc69e7";
  };

  meta = with pkgs.lib; {
    description = "Zig";
    homepage = "https://ziglang.org/";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
