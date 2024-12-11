{ config, lib, pkgs, ... }:

{
  # https://lobste.rs/s/xyvwux/what_are_your_favorite_non_standard_cli
  home.packages = with pkgs; [ ncdu tree aerc gron nh ugrep svu pv doggo fx ];
}
