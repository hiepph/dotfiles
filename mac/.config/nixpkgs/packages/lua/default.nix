{ pkgs ? import <nixpkgs> {} }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "lua";
  version = "5.4.4";

  src = fetchTarball {
    url = "https://www.lua.org/ftp/lua-${version}.tar.gz";
    sha256 = "035fwx04037lgrldkppp3jz689ck85nyxd7fplhnqj68mc44phb8";
  };

  buildInputs = [
    gcc
    readline
  ];

  buildPhase = ''
make all
'';

  testPhase = ''
make test
'';

  installPhase = ''
mkdir -p $out/bin
mv src/{lua,luac} $out/bin
'';

  meta = with lib; {
    description = "Lua programming language";
    homepage = "https://www.lua.org";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.all;
  };
}
