{ config, lib, pkgs, ... }:

let
  configHome = if pkgs.stdenv.isDarwin then
    "Library/Application Support"
  else
    config.xdg.configHome;
in {
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "claude-code" ];
  home.packages = with pkgs; [
    claude-code
    # llm.withPlugins has been deprecated
    (pkgs.callPackage ../packages/llm-with-plugins { })
  ];

  home.file."${configHome}/io.datasette.llm/default_model.txt".text =
    "anthropic/claude-3-5-haiku-latest";
}
