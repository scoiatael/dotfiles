{ den, inputs, ... }:
{
  # mcp for nono itself and the profiles claude-sandboxed needs.
  den.aspects.claude.includes = [
    den.aspects.llm-agents
    den.aspects.mcp
  ];

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
      # nono plus --rollback is the boundary inside the sandbox, so the flag
      # pins auto there regardless of what project settings ask for.
      claude-sandboxed = pkgs.writeShellScriptBin "claude-sandboxed" ''
        exec ${lib.getExe' nono "nono"} run \
          --rollback \
          --profile claude-sandboxed \
          -- \
          ${lib.getExe' config.programs.claude-code.finalPackage "claude"} \
          --permission-mode auto "$@"
      '';
    in
    {
      home.packages = [
        claude-sandboxed
        self'.packages.claude-url-allowlist
        self'.packages.claude-bash-guard
        self'.packages.claude-statusline
      ];

      # The base policy claude-sandboxed extends. The `mcp' aspect owns
      # ~/.config/nono/profiles and merges this in as claude.json.
      nono.extraPacks.claude = "${nono-packs}/claude";

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

          permissions = {
            defaultMode = "auto";

            additionalDirectories = [
              "${config.home.homeDirectory}/dotfiles"
              "${config.home.homeDirectory}/Documents/worktrees"
            ];

            # [[id:caabd499-2344-4dd7-a9de-72fe04af0a49][llm-codegraph]]
            # MCP servers reach claude through home-manager's generated plugin, hence the prefix.
            allow = [ "mcp__plugin_claude-code-home-manager_codegraph__*" ];

            # No deny rules: commits and writes into ~ depend on the directory,
            # which a rule cannot match, so the hook below decides both.
          };

          statusLine = {
            type = "command";
            command = "claude-statusline";
          };

          # Keeps files out of ~ itself — for the file tools by path, for Bash
          # by parsing the command — keeps commits to linked worktrees, and
          # refuses commands with more moving parts than it can account for.
          hooks.PreToolUse = [
            {
              matcher = "Bash|Write|Edit|NotebookEdit";
              hooks = [
                {
                  type = "command";
                  command = "claude-bash-guard";
                }
              ];
            }
          ];

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
