{ config, lib, pkgs, ... }:
let
  processComposeConfigDir = if pkgs.stdenv.isDarwin then
    "Library/Application Support"
  else
    config.xdg.configHome;
in {
  # https://lobste.rs/s/xyvwux/what_are_your_favorite_non_standard_cli
  home.packages = with pkgs;
    [
      mosh
      choose
      ouch
      pinact # pin github actions
      tree
      gron # [[id:dbe828b1-9291-4249-8cb0-b74bac8aade5][gron]]
      nh # [[id:72d47c79-93c8-4ce0-8452-8a386f4a9993][nh]]
      ugrep # [[id:a92dcd13-5bb1-4dc7-b47c-0cef5b9affe5][ugrep]]
      svu # [[id:e04c2493-a08f-4715-8567-3e75337a1709][svu]]
      pv # [[id:429e6a36-21ba-4dd3-9a6f-fa544c728b7a][pv]]
      doggo
      fx
      gitu
      spacer
      tailspin
      oha
      (pkgs.callPackage ../packages/cometary { })
      (pkgs.callPackage ../packages/human { })
      (pkgs.callPackage ../packages/mvbak { })
    ] ++ (lib.lists.optional (with pkgs.stdenv.hostPlatform;
      isDarwin -> lib.versionAtLeast darwinSdkVersion "11.0") ncdu);

  home.file."${processComposeConfigDir}/process-compose/theme.yaml".source =
    ../config/process-compose/catppuccin-frappe.yaml;
  home.file."${processComposeConfigDir}/process-compose/settings.yaml".source =
    ../config/process-compose/settings.yaml;
  home.file."${processComposeConfigDir}/process-compose/shortcuts.yaml".source =
    ../config/process-compose/shortcuts.yaml;
}
