{ config, lib, pkgs, ... }: {
  home.language = { base = "en_GB.UTF-8"; };

  home.packages = with pkgs; [
    coreutils-full
    patchutils
    html-tidy
    wget
    neofetch
    curlie
    xh
    dive
    gnutar
    tokei
    du-dust
    fd
    jq
    ripgrep
    units
    lsof
    ruby_3_3
    sops
    viddy
    hyperfine
    file
    imagemagick
    yarn # for yarn create
    unzip
    zip
    pwgen
    yubikey-manager
    pry
    restic
    rsync
    stylua
    (callPackage ../packages/gitblame { })
    (callPackage ../packages/inflector-rs { })
    (callPackage ../packages/indices { })
    nixd

    jetbrains-mono
    dejavu_fonts
    noto-fonts-color-emoji
    unifont
    udev-gothic

    czkawka-full
  ];

  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  programs = {
    gpg = { enable = true; };
    starship = {
      enable = true;
      settings = {
        format = ''
          $cmd_duration┌$fill$git_branch$git_stateǂ$directory
          └$username$git_status$character'';

        command_timeout = 90;

        fill = {
          symbol = "─";
          style = "bold green";
        };

        directory = {
          style = "blue";
          truncate_to_repo = false;
        };

        character = {
          success_symbol = "[λ](purple)";
          error_symbol = "[λ](red)";
          vicmd_symbol = "[❮](green)";
        };

        git_branch = {
          format = "[$branch]($style)";
          style = "bright-black";
        };

        git_status = {
          format = "[\\($ahead_behind$staged$modified\\)]($style)";
          style = "cyan";
          conflicted = "[䷅](red)";
          untracked = "[·](grey)";
          modified = "[·](yellow)";
          stashed = ""; # not interesting, keep empty
          staged = "[·](green)";
        };

        git_state = {
          format =
            "\\([|$state( $progress_current/$progress_total)]($style)\\)";
          style = "bright-black";
        };

        cmd_duration = {
          format = ''
            [  $duration
            ]($style)'';
          style = "yellow";
        };
      };
    };
    dircolors = { enable = true; };
    bat = {
      enable = true;
      themes = {
        catpuccin = {
          src = pkgs.fetchFromGitHub {
            owner = "catppuccin";
            repo = "bat";
            rev = "d2bbee4f7e7d5bac63c054e4d8eca57954b31471";
            sha256 = "sha256-x1yqPCWuoBSx/cI94eA+AWwhiSA42cLNUOFJl7qjhmw=";
          };
          file = "themes/Catppuccin Frappe.tmTheme";
        };
      };
      config = {
        paging = "never";
        theme = "catpuccin";
      };
      extraPackages = with pkgs.bat-extras; [ batman ];
    };
    btop = { enable = true; };
    atuin = {
      enable = true;
      flags = [ "--disable-up-arrow" ];
    };
    eza = { enable = true; };
    fzf = { enable = true; };
    zoxide = { enable = true; };
    direnv = {
      enable = true;
      nix-direnv = { enable = true; };
      stdlib = "source ${../config/direnvrc}";
    };
  };

  home.file.".envrc".text = ""; # for direnv to load in HOME
  home.file.".direnvrc".source =
    ../config/direnvrc; # for direnv to load in HOME

  home.file.".zprofile".text = ''
    export DOOMLOCALDIR="~/.emacs.local"
    export EMACSDIR="~/.emacs.doom"
  '';

  home.file.".pryrc".text = ''
    Pry::Prompt.add(
      :vterm,
      "A simple `>>` w/ vterm ending.",
      ['>> ', ' | ']
    ) do |_, _, _, sep|
      whoami = ENV['USER']
      pwd = ENV['pwd']
      sep + "\e]51;A#{whoami}@:#{pwd}\e\\"
    end

    Pry.config.prompt = Pry::Prompt[:vterm]
  '';

  home.file.".gnupg/dirmngr.conf".text = ''
    keyserver hkps://keys.openpgp.org/
  '';

  home.file.".gnupg/gpg-agent.conf" =
    let program = "/run/current-system/sw/bin/pinentry-mac";
    in lib.mkIf pkgs.stdenv.isDarwin { text = "pinentry-program ${program}"; };

  home.activation.initPass = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    if ! test -d ~/.password-store/; then
      GPG="$(gpg -K --with-colons | grep fpr | head -n 1 | ${pkgs.choose}/bin/choose -f : -1)"
      if test -n "$GPG"; then
         echo "Creating password store with gpg=$GPG"
         ${pkgs.pass}/bin/pass init "$GPG"
      fi
    fi
  '';
}
