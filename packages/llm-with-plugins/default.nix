{ lib, pkgs, ... }:

let
  llm-commit = pkgs.python3Packages.callPackage ../llm-commit { };
  pythonEnv =
    pkgs.python3.withPackages (pp: with pp; [ llm llm-anthropic llm-commit ]);
  pass = lib.getExe pkgs.pass;
in pkgs.writeShellScriptBin "llm" ''
  unset PYTHONPATH # Otherwise it breaks in Python devenvs
  export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"
  exec ${pythonEnv}/bin/llm "''${@}"
''
