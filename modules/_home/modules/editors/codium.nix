{ lib, pkgs, ...}:
{
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      userSettings = {
        "update.mode" = "none";
        "[nix]"."editor.tabSize" = 2;
        "workbench.startupEditor" = "none";
        "editor.fontFamily" = "JetBrainsMono Nerd Font";
        "editor.fontSize" = 12;
        "workbench.colorTheme" = "GitHub Dark Dimmed";
        "[typescript]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
        "extensions.autoUpdate" = false;
      };
      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        github.github-vscode-theme
        esbenp.prettier-vscode
        mattn.lisp
        tamasfe.even-better-toml
        kamadorueda.alejandra
      ];
      mutableExtensionsDir = false;
    };
}
