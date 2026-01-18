{ pkgs, ... }:

let pythonEnv = pkgs.python3.withPackages (pp: with pp; [ markitdown ]);
in pkgs.writeShellScriptBin "markitdown" ''
  exec ${pythonEnv}/bin/markitdown "''${@}"
''
