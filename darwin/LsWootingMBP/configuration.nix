{
  pkgs,
  home-manager,
  self,
  ...
}:

{
  imports = [
    home-manager.darwinModules.home-manager
    ../../modules/darwin/default.nix
    ../../modules/darwin/aerospace.nix
    ../../modules/darwin/sketchybar.nix
    ../../modules/darwin/lix.nix
  ];

  system.primaryUser = "lukas";

  users.users.lukas = {
    name = "lukas";
    home = "/Users/lukas";
  };

  # hardware.notch = false;
  homebrew = {
    casks = [
      "bitwarden"
      "slack"
      "google-drive"
      "monodraw"
      "altair-graphql-client"
      "forklift"
      "mitmproxy"
      "astropad-studio"
      "utm"
      "notion"
      "thebrowsercompany-dia"
      "tailscale-app"
      "eqmac"
    ];
    brews = [ "stlink" ];
  };
  nix.linux-builder = {
    enable = true;
    systems = [ "x86_64-linux" ];
    package = pkgs.darwin.linux-builder-x86_64;
  };
  #  nix.linux-builder = {
  #   enable = true;
  #  systems = [ "x86_64-linux" "aarch64-linux" ];
  #  config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  # };
  # nix.settings.trusted-users = [ "@admin" ];
  # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
  # TODO: these break some thing
  # read up on them.
  #home-manager.useGlobalPkgs = true;
  #home-manager.useUserPackages = true;
  home-manager.users.lukas.imports = [ ../../home/lukas-LsWootingMBP/configuration.nix ];
}
