{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "terraform";
  version = "1.2.2";

  buildInputs = [
    pkgs.unzip
  ];

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];

  src = pkgs.fetchurl {
    url = "https://releases.hashicorp.com/${pname}/${version}/${pname}_${version}_darwin_amd64.zip";
    sha256 = "bd224d57718ed2b6e5e3b55383878d4b122c6dc058d65625605cef1ace9dcb25";
  };

  unpackPhase = ''
unzip $src
'';

  installPhase = ''
mkdir -p $out/bin
chmod +x ./terraform
mv ./terraform $out/bin
'';

  meta = with pkgs.lib; {
    description = "Terraform";
    homepage = "https://www.terraform.io/";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
