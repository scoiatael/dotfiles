{ fastanime, ... }:
{ pkgs, ... }: {
  home.packages = [
    pkgs.mpv
    # HACK: plyer required by upstream is broken on macOS
    # HACK: thefuzz required by upstream is broken on macOS
    (fastanime.packages."${pkgs.stdenv.hostPlatform.system}".default.overrideAttrs {
      preBuild = ''
        sed -i 's/rich>=13.9.2/rich>=13.8.1/' pyproject.toml
        sed -i 's/pycryptodome>=3.21.0/pycryptodome>=3.20.0/' pyproject.toml
      '';
      propagatedBuildInputs = with pkgs.python312.pkgs; [
        click
        inquirerpy
        requests
        rich
        yt-dlp
        dbus-python
        hatchling
        mpv
        fastapi
        pycryptodome
        pypresence
        (thefuzz.overrideAttrs {
          propagatedBuildInputs = [
            (levenshtein.overrideAttrs {
              src = pkgs.fetchFromGitHub {
                owner = "maxbachmann";
                repo = "Levenshtein";
                tag = "v0.27.0";
                hash = "sha256-kiYu46qv8sBBcPoCo3PN1q9F0EJ1s5hAMKavPaztM4s=";
                fetchSubmodules = true; # # for vendored `rapidfuzz-cpp`
              };
            })
          ];
        })
      ];
    })
  ];
}
