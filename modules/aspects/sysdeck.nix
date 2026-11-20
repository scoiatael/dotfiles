{ den, ... }:
{
  den.aspects.sysdeck = {
    includes = [ den.aspects.plasma-manager ];
    homeManager =
      { pkgs, self', ... }:
      {
        home.packages = [
          self'.packages.sysdeck
          pkgs.wl-clipboard
        ];

        xdg.configFile."sysdeck/config.toml".text = ''
          [sources.sway]
          enabled = false
        '';

        wayland.windowManager.niri.settings = {
          binds."Mod+D".spawn = "sysdeck";
          binds."Mod+Shift+D".spawn = [
            "sysdeck"
            "--clipboard-history"
          ];
          spawn-sh-at-startup = "wl-paste --watch sysdeck --clipboard-capture";
        };

        programs.plasma = {
          hotkeys.commands.sysdeck = {
            name = "sysdeck";
            key = "Meta+D";
            command = "sysdeck";
          };
          hotkeys.commands.sysdeck-clipboard = {
            name = "sysdeck clipboard";
            key = "Meta+Shift+D";
            command = "sysdeck --clipboard-history";
          };
          # Meta+D is KWin's "Peek at Desktop" by default.
          shortcuts.kwin."Show Desktop" = [ ];
          startup.startupScript.sysdeck-clipboard = {
            text = "wl-paste --watch sysdeck --clipboard-capture >/dev/null &";
            runAlways = true;
          };
        };
      };
  };
}
