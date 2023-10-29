{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.tmuxPlugins.mkTmuxPlugin rec {
  pluginName = "tmux-colortag";
  version = "0.0.0";
  rtpFilePath = "tmux-colortag.tmux";

  src = pkgs.fetchFromGitHub {
    owner = "Determinant";
    repo = pluginName;
    rev = "8cad56d";
    sha256 = "sha256-lLe6dk9qyOboM9pnZ3xZ4lyb+hxBMzc2yuqjq4wlxBg=";
  };

  postInstall = ''
    sed -i -e 's|TMUX_PLUGIN_MANAGER_PATH|TMPDIR|g' $target/name2color.py
    sed -i -e 's|/usr/bin/env python|${pkgs.python38}/bin/python|g' $target/name2color.py
  '';
}
