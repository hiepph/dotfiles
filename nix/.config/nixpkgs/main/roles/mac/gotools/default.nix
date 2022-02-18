{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  pname = "gotools";
  version = "1.16.9";

  # choose your package here
  # each package would lead to "go install <package>"
  packages = [
    "golang.org/x/tools/cmd/goimports@latest"
    "github.com/go-delve/delve/cmd/dlv@latest"
  ];

  dontUnpack = true;

  buildInputs = [
    pkgs.go
  ];

  installPhase = ''
export GOPATH=$PWD/go
export GOCACHE=$PWD/cache
mkdir -p $GOPATH
mkdir -p $GOCACHE

for package in $packages; do
    go install $package
done

mkdir -p $out/bin
mv $GOPATH/bin/* $out/bin
'';

  postInstall = ''
rm -rf $GOCACHE
'';

  meta = with pkgs.lib; {
    description = "Go with some useful packages";
    maintainers = [ maintainers.hiepph ];
    platforms = platforms.darwin;
  };
}
