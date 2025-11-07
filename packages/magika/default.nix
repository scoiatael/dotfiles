{ config, lib, pkgs, ... }:

# Adding magika directly results in errors on macOS as of 2025-07-11
pkgs.writeShellScriptBin "magika" ''
  unset PYTHONPATH # Otherwise it breaks in Python devenvs
  exec ${pkgs.magika}/bin/magika-python-client "''${@}"
''

