{
  dotfiles,
  inputs,
  lib,
  pkgs,
  ...
}:

rec {
  pass = lib.getExe pkgs.pass;
  inherit (pkgs.stdenv.hostPlatform) system;
  companionPrompt = dotfiles.config."companion/prompt.md";
  gh = lib.getExe pkgs.gh;
  nono = inputs.llm-agents.packages.${system}.nono;
  nonoExe = lib.getExe' nono "nono";
  resolveGhToken = ''
    GH_TOKEN="$(${pass} show github-com-readonly-token 2>/dev/null | head -1)"
    if [ -z "$GH_TOKEN" ]; then
      GH_TOKEN="$(${gh} auth token 2>/dev/null)"
    fi
    [ -n "$GH_TOKEN" ] && export GH_TOKEN || unset GH_TOKEN
  '';
  # The wrap both modes of a launcher share, so the ACP server and the
  # terminal session can never drift apart on what they may touch.
  # --allow-cwd is mandatory with the profile's `interactive: false';
  # --workdir expands $WORKDIR in it, which grants write to the project's
  # .codegraph/ -- SQLite needs to create WAL/journal files beside the DB
  # even to read it.
  wrapArgs = profile: "wrap --profile ${profile} --allow-cwd --workdir \"$PWD\" --";
}
