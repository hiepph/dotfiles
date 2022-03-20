{ pkgs ? import <nixpkgs> {} }:

with pkgs.python38Packages;
buildPythonPackage rec {
  pname = "black";
  version = "22.1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1ddgh0da9kqdmzypkvi9srfjxs8mg75pbrab2z0nypv36lnikh57";
  };
  propagatedBuildInputs = [
    click
    mypy-extensions
    pathspec
    platformdirs
    tomli
    typed-ast
    typing-extensions
  ];
  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/psf/black";
    description = "The uncompromising Python code formatter";
    maintainers = [ maintainers.hiepph ];
  };
}
