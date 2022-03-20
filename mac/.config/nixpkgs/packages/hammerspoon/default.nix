{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "hammerspoon";
  version = "0.9.93";

  src = pkgs.fetchzip {
    url = "https://github.com/Hammerspoon/hammerspoon/releases/download/${version}/Hammerspoon-${version}.zip";
    sha256 = "0ryfn1drpyyvqxx1f7c6sciywhldsi1mf3zzimzvb821rhs4ik1q";
  };

  installPhase = ''
mkdir -p $out/Applications/Hammerspoon.app
mv ./* $out/Applications/Hammerspoon.app

chmod +x $out/Applications/Hammerspoon.app/Contents/MacOS/Hammerspoon
'';

  meta = with pkgs.lib; {
    description = "Desktop automation tool for OS X";
    homepage = "https://www.hammerspoon.org";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
