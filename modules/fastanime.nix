{ fastanime, ... }:
{ pkgs, lib, ... }: {
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [ uosc thumbfast ];
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
  home.file."Library/Application Support/FastAnime/config.ini".source =
    ../config/fastanime/config.ini;
  xdg.configFile."mpv/script-opts/uosc.conf".source = ../config/mpv/uosc.conf;
  xdg.configFile."mpv/fonts".source = pkgs.symlinkJoin {
    name = "mpv-uosc-fonts";
    paths = [
      "${pkgs.mpvScripts.uosc}/share/fonts"
      (pkgs.stdenvNoCC.mkDerivation {
        name = "material-design-iconic-font";

        src = pkgs.fetchzip {
          url =
            "https://github.com/zavoloklom/material-design-iconic-font/releases/download/2.2.0/material-design-iconic-font.zip";
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
    # HACK: plyer required by upstream is broken on macOS
    # HACK: thefuzz required by upstream is broken on macOS
    # HACK: login doesn't work on macOS
    (fastanime.packages."${pkgs.stdenv.hostPlatform.system}".default.overrideAttrs {
      preBuild = ''
        sed -i 's/rich>=13.9.2/rich>=13.8.1/' pyproject.toml
        sed -i 's/pycryptodome>=3.21.0/pycryptodome>=3.20.0/' pyproject.toml

        sed -i 's/, wait=True/, wait=False/' fastanime/cli/commands/anilist/login.py
        sed -i 's/Prompt.ask("Enter token")/open(Prompt.ask("Enter filename with token: "), "r").read().strip()/' fastanime/cli/commands/anilist/login.py
      '';
      propagatedBuildInputs = with pkgs.python312.pkgs; [
        click
        inquirerpy
        requests
        rich
        yt-dlp
        dbus-python
        hatchling
        fastapi
        pycryptodome
        pypresence
        thefuzz
      ];
    })
  ];
}
