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
      tokei # [[id:6119b4ab-d542-4a83-91a4-e65e02642443][tokei]]
      mosh # [[id:3ef5214a-337e-485d-997f-ef9dfb9c7539][mosh]]
      choose # [[id:6d09c895-e12b-4230-894d-691e34d91913][choose]]
      ouch # [[id:e7780a1a-1c8a-40bd-a80f-4ca4077d84d3][ouch]]
      pinact # [[id:373b0dca-999d-4018-8541-080212684003][pinact]]
      tree # [[id:1b6a2a6d-c2ca-4cb6-8124-163b67f82570][tree]]
      gron # [[id:dbe828b1-9291-4249-8cb0-b74bac8aade5][gron]]
      nh # [[id:72d47c79-93c8-4ce0-8452-8a386f4a9993][nh]]
      ugrep # [[id:a92dcd13-5bb1-4dc7-b47c-0cef5b9affe5][ugrep]]
      svu # [[id:e04c2493-a08f-4715-8567-3e75337a1709][svu]]
      pv # [[id:429e6a36-21ba-4dd3-9a6f-fa544c728b7a][pv]]
      doggo # [[id:6ce78a90-39c3-4065-bee8-344ece1c702b][doggo]]
      tealdeer # [[id:d069b405-6ea1-4742-89e8-7b4db610c696][tealdeer]]
      fx # [[id:631d0161-8081-4c64-8356-9c8391142dd6][fx]]
      yq # [[id:bd1d7422-0b72-4af4-a3fe-64b32ee727c5][yq]]
      gitu # [[id:4d118287-a703-4845-89b6-7cd14ee47390][gitu]]
      spacer # [[id:9e83cbf2-83f4-4cda-90ea-3363736a07e9][spacer]]
      tailspin # [[id:8042e9e2-b2cb-4c24-bd34-64cc6b56aac7][tailspin]]
      oha # [[id:607d4762-4482-47b8-a9c2-c5c7f5ec113c][oha]]
      (pkgs.callPackage ../packages/magika
        { }) # [[id:10fe56e4-553b-4d19-ab1f-9c12287e81d4][magika]]
      (pkgs.callPackage ../packages/cometary
        { }) # [[id:09791008-55e2-47e7-9100-53ee1a1ae6bd][cometary]]
      (pkgs.callPackage ../packages/human
        { }) # [[id:802d01e4-e1f0-4843-aa4e-53943330e763][human]]
      (pkgs.callPackage ../packages/mvbak
        { }) # [[id:80944be2-1ab0-4400-a143-f73deaab1522][mvbak]]
    ] ++ (lib.lists.optional (with pkgs.stdenv.hostPlatform;
      isDarwin -> lib.versionAtLeast darwinSdkVersion "11.0")
      ncdu); # [[id:3142c82d-fc4d-4467-8190-f2aea83677d7][ncdu]]

  home.file."${processComposeConfigDir}/process-compose/theme.yaml".source =
    ../config/process-compose/catppuccin-frappe.yaml;
  home.file."${processComposeConfigDir}/process-compose/settings.yaml".source =
    ../config/process-compose/settings.yaml;
  home.file."${processComposeConfigDir}/process-compose/shortcuts.yaml".source =
    ../config/process-compose/shortcuts.yaml;
}
