{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "reflex";
  version = "0.3.1";

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/bin
cp reflex_darwin_amd64/reflex $out/bin
'';
  src = pkgs.fetchurl {
    url = "https://github.com/cespare/reflex/releases/download/v${version}/reflex_darwin_amd64.tar.gz";
    sha256 = "15718awcp82r09a8zqczj22rvqfv44sfay584ba8ymfvv057ffgh";
  };

  meta = with pkgs.lib; {
    description = "Reflex - run a command when files change";
    homepage = "https://github.com/cespare/reflex";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
