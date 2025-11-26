{ config, lib, pkgs, ... }:

let
  configHome = if pkgs.stdenv.isDarwin then
    "Library/Application Support"
  else
    config.xdg.configHome;
  pdf-convert = pkgs.writeShellScriptBin "pdf-to-jpg" ''
    convert -density 300 "$1" -quality 100 "''${1%.*}.jpg"
    echo "''${1%.*}.jpg"
  '';
  llm-invoice = pkgs.writeShellScriptBin "llm-invoice" ''
    llm -t invoice -a "$1" "$1"
  '';
in {
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "claude-code" "amp-cli" ];
  home.packages = with pkgs; [
    # llm.withPlugins has been deprecated
    (pkgs.callPackage ../packages/llm-with-plugins { })

    # for PDF conversion
    ghostscript
    pdf-convert
    llm-invoice

    # claude-code
    # # Amp
    # amp-cli
  ];

  home.file."${configHome}/io.datasette.llm/default_model.txt".text =
    "anthropic/claude-sonnet-4-0";

  home.activation.linkLLMTemplates =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      [ ! -L "${configHome}/io.datasette.llm/templates" ] && ln -sf "${config.home.homeDirectory}/dotfiles/config/llm/templates" "${configHome}/io.datasette.llm/templates"
    '';
}
