{ lib, pkgs, ...}:
{
  programs.alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "JetBrainsMono Nerd Font"; };
          size = 12;
        };
        terminal = { shell = { program = "${pkgs.zsh}/bin/zsh"; }; };
        window.decorations = "None";
        # import =
        #  [ "${config.home.homeDirectory}/.config/alacritty/theme.toml" ];
      };
    };
}
