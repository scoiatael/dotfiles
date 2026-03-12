{
  pkgs,
  nix-vscode-extensions,
  cursorless_talon,
  talonhub_community,
  system,
  ...
}: {
  programs.vscode.extensions = with nix-vscode-extensions.extensions.${pkgs.stdenv.system}.open-vsx; [
    pokey.cursorless
    pokey.parse-tree
    pokey.talon
  ];
  home.file.".talon/user/community".source = talonhub_community;
  home.file.".talon/user/cursorless-talon".source = cursorless_talon;
}
