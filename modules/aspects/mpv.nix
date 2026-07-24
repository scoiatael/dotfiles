{
  den.aspects.mpv = {
    homeManager =
      {
        pkgs,
        ...
      }:

      {
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [
            uosc
            thumbfast
          ];
          config = {
            osd-bar = "no";
            border = "no"; # Optional, but recommended
          };
          scriptOpts = {
            thumbfast = {
              network = "yes";
              spawn_first = "yes";
            };
          };
        };
        xdg.configFile."mpv/script-opts/uosc.conf".source = ../../config/mpv/uosc.conf;
        xdg.configFile."mpv/fonts".source = pkgs.symlinkJoin {
          name = "mpv-uosc-fonts";
          paths = [
            "${pkgs.mpvScripts.uosc}/share/fonts"
            (pkgs.stdenvNoCC.mkDerivation {
              name = "material-design-iconic-font";

              src = pkgs.fetchzip {
                url = "https://github.com/zavoloklom/material-design-iconic-font/releases/download/2.2.0/material-design-iconic-font.zip";
                hash = "sha256-xYoJjzxnjnCXZES7UVhNsk3T9MazK1KlNFzcTBsWv+M=";
                stripRoot = false;
              };

              phases = [ "installPhase" ];

              installPhase = ''
                mkdir -p $out/
                cp $src/fonts/Material-Design-Iconic-Font.ttf $out/
              '';
            })
          ];
        };
        home.packages = [
          pkgs.chafa # for previews
          pkgs.yt-dlp
        ];
      };
  };
}
