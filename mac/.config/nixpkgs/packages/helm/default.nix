{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "helm";
  version = "3.8.2";

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
mkdir -p $out/bin
cp darwin-amd64/helm $out/bin
'';
  src = pkgs.fetchurl {
    url = "https://get.helm.sh/helm-v${version}-darwin-amd64.tar.gz";
    sha256 = "25bb4a70b0d9538a97abb3aaa57133c0779982a8091742a22026e60d8614f8a0";
  };

  meta = with pkgs.lib; {
    description = "Helm - The package manager for Kubernetes";
    homepage = "https://helm.sh/";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
