{
  den.aspects.canario = {
    homeManager =
      { lib, ... }:
      {
        # rio sets TERM=xterm-rio, which many programs don't recognise
        programs.zsh.envExtra = lib.mkAfter ''
          [[ "$TERM" == "xterm-rio" ]] && export TERM=xterm-256color
        '';
        programs.fish.interactiveShellInit = lib.mkAfter ''
          if test "$TERM" = xterm-rio
            set -gx TERM xterm-256color
          end
        '';
      };
    darwin = {
      homebrew.casks = [ "canario" ];
    };
  };
}
