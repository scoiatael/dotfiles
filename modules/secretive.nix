{ config, lib, pkgs, ... }:

{
  programs.zsh.initContent = ''
    export SSH_AUTH_SOCK=$HOME/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
    if ! test -S $SSH_AUTH_SOCK; then
      print -P "%F{red}[secretive] agent is not running%F{reset_colors}"
    fi
  '';
}
