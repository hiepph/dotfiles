{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "julia-mac";
  version = "1.6.3";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/Applications
cp -r Julia-1.6.app $out/Applications/Julia.app

mkdir -p $out/bin
ln -s $out/Applications/Julia.app/Contents/Resources/julia/bin/julia $out/bin/julia
'';
  src = pkgs.fetchurl {
    name = "${pname}-${version}.dmg";
    url = "https://julialang-s3.julialang.org/bin/mac/x64/1.6/julia-${version}-mac64.dmg";
    sha256 = "1pw5m6yplvncqgvx7nkxzscf97r0wkxvpys7l1vk1qnnkwdyzi9i";
  };

  meta = with pkgs.lib; {
    description = "Julia";
    homepage = "https://julialang.org/";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
