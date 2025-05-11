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
      gron
      nh
      ugrep
      svu
      pv
      doggo
      fx
      gitu
      (pkgs.callPackage ../packages/cometary { })
      (pkgs.callPackage ../packages/human { })
      (pkgs.callPackage ../packages/mvbak { })
    ] ++ (lib.lists.optional (with pkgs.stdenv.hostPlatform;
      isDarwin -> lib.versionAtLeast darwinSdkVersion "11.0") ncdu);

  home.file."${processComposeConfigDir}/process-compose/theme.yaml".source =
    ../config/process-compose/catppuccin-frappe.yaml;
  home.file."${processComposeConfigDir}/process-compose/settings.yaml".source =
    ../config/process-compose/settings.yaml;
}
