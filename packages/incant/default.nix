# @ts: { pkgs: Pkgs }
{ pkgs, ... }:

pkgs.python3Packages.buildPythonApplication rec {
  pname = "incus-incant";
  version = "0.5";
  pyproject = true;

  src = pkgs.fetchPypi {
    pname = "incus_incant";
    inherit version;
    sha256 = "93a48f52d344c1e65230fa0859b736d2ae61020a86dc6c382741b0335bbed08f";
  };

  build-system = [ pkgs.python3Packages.poetry-core ];
  dependencies = with pkgs.python3Packages; [
    click
    jinja2
    pyyaml
    mako
  ];

  meta.mainProgram = "incant";
}
