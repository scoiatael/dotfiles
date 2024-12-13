{ config, lib, pkgs, ... }:

{
  # https://lobste.rs/s/xyvwux/what_are_your_favorite_non_standard_cli
  home.packages = with pkgs;
    [
      tree
      aerc
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
    ] ++ (lib.lists.optional (with pkgs.stdenv.hostPlatform;
      isDarwin -> lib.versionAtLeast darwinSdkVersion "11.0") ncdu);
}
