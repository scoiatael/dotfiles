{ pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    with pkgs; [ vim
                 pinentry-mac
    ];

  services.spacebar = {
    enable = true;
    package = pkgs.spacebar;
  };

  # programs.fish.enable = true;
  programs.zsh.enable = true;  # default shell on catalina
  programs.gnupg = {
    agent = {
     enable= true; 
enableSSHSupport = true;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.package = pkgs.nix;
}
