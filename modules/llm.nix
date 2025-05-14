{ config, lib, pkgs, ... }:

let
  configHome = if pkgs.stdenv.isDarwin then
    "Library/Application Support"
  else
    config.xdg.configHome;
  pdf-convert = pkgs.writeShellScriptBin "pdf-to-jpg" ''
    convert -density 300 "$1" -quality 100 "''${1%.*}.jpg"
  '';
in {
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "claude-code" ];
  home.packages = with pkgs; [
    claude-code
    # llm.withPlugins has been deprecated
    (pkgs.callPackage ../packages/llm-with-plugins { })

    # for PDF conversion
    ghostscript
    pdf-convert
  ];

  home.file."${configHome}/io.datasette.llm/default_model.txt".text =
    "anthropic/claude-3-5-haiku-latest";

  home.activation.linkLLMTemplates =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      [ ! -L "${configHome}/io.datasette.llm/templates" ] && ln -sf "${config.home.homeDirectory}/dotfiles/config/llm/templates" "${configHome}/io.datasette.llm/templates"
    '';
}
