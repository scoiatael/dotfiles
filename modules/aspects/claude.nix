{ den, inputs, ... }:
{
  den.aspects.claude.includes = [ den.aspects.llm-agents ];

  den.aspects.claude.homeManager =
    {
      config,
      lib,
      pkgs,
      self',
      ...
    }:
    let
      nono-packs = pkgs.fetchFromGitHub {
        owner = "nolabs-ai";
        repo = "nono-packs";
        rev = "22fabe9410d5df83058ce7e830b016b81c1931a2";
        hash = "sha256-F55fESmCvtxZcJt9z9iZ5sptPLu8s5/t5Dn+itgSU2E=";
      };
      nono = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.nono;
      claude-sandboxed = pkgs.writeShellScriptBin "claude-sandboxed" ''
        exec ${lib.getExe' nono "nono"} run \
          --rollback \
          --profile claude-sandboxed \
          --
          ${lib.getExe' config.programs.claude-code.finalPackage "claude"} "$@"
      '';
    in
    {
      home.packages = [
        claude-sandboxed
        # PreToolUse hook helper for whitelisting URLs in project settings
        self'.packages.claude-url-allowlist
      ];

      xdg.configFile."nono/profiles".source = pkgs.buildEnv {
        name = "nono-profiles";
        paths = [ ../../config/nono ];
        postBuild = ''
          ln -s "${nono-packs}/claude/policy.json" $out/claude.json
        '';
      };

      programs.git.ignores = lib.mkAfter [ "/.claude/settings.local.json" ];

      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;

        plugins = [ "${nono-packs}/claude" ];

        package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code.overrideAttrs {
          disableTelemetry = true;
        };

        context = ../../config/claude/CLAUDE.md;
        skills = ../../config/claude/skills;

        settings = {
          tui = "default";
          spinnerTipsEnabled = false;

          enabledPlugins = {
            "clangd-lsp@claude-plugins-official" = true;
          };

          # [[id:caabd499-2344-4dd7-a9de-72fe04af0a49][llm-codegraph]]
          permissions.allow = [ "mcp__codegraph__*" ];

          hooks.UserPromptSubmit = [
            {
              hooks = [
                {
                  type = "command";
                  command = "codegraph prompt-hook";
                }
              ];
            }
          ];
        };
      };
    };
}
