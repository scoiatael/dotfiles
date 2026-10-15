{
  inputs,
  dotfiles,
  den,
  ...
}:

{
  # A read-only companion agent, sandboxed by nono. It searches and suggests
  # while the user works; it cannot change their files.
  #
  # Two interchangeable backends, same shape each time: an outer launcher that
  # resolves the API key before entering the sandbox, a config tree that drops
  # the write tools, and the shared prompt in config/companion/prompt.md.
  # `scoiatael/companion-backend' in the doom `scoiatael/llm' module picks
  # which launcher agent-shell spawns, so switching needs no rebuild.
  #
  # Each launcher has two modes. `maki-companion acp' / `claude-companion acp'
  # is the ACP server Emacs (agent-shell) drives; with any other arguments the
  # same sandbox and config run that backend's own terminal UI -- `maki' and
  # `claude' respectively -- so the companion is usable outside Emacs too.
  #
  # [[id:b842d083-9f81-44d4-b3e6-f140c32a8bee][llm-maki-companion]]
  den.aspects.companion.includes = [
    # den.aspects.maki
    den.aspects.llm-agents
  ];

  den.aspects.companion.homeManager =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit
        (import ./_companion/default.nix {
          inherit
            dotfiles
            inputs
            lib
            pkgs
            ;
        })
        pass
        companionPrompt
        wrapArgs
        resolveGhToken
        nonoExe
        ;
      jsonFormat = pkgs.formats.json { };

      claude-agent-acp = inputs.llm-agents.packages.${system}.claude-agent-acp;
      claude-code = inputs.llm-agents.packages.${system}.claude-code;

      ## claude-agent-acp backend

      # CLAUDE_CONFIG_DIR. settings.json, CLAUDE.md and skills/ are linked in
      # below, but the directory itself has to stay a real writable one: it is
      # also where the CLI keeps session transcripts, todos and shell
      # snapshots. Hence individual `home.file' entries rather than a single
      # store symlink for the whole tree.
      claudeStateDir = ".local/state/claude-companion";

      claudeCompanionSettings = jsonFormat.generate "claude-companion-settings.json" {
        # Belt to the sandbox's braces, and to the `disallowedTools' the Emacs
        # side sends: deny rules also cover Task subagents.
        permissions = {
          deny = [
            "Write"
            "Edit"
            "NotebookEdit"
          ];
          # nono and the deny rules are the boundary; prompts on top of them
          # only stall the ACP stream, which has no good place to show them.
          defaultMode = "auto";
        };
        # Claude Code can sandbox Bash itself with seatbelt. Nesting that
        # inside nono's seatbelt sandbox buys nothing and can only break it --
        # nono is the boundary here.
        sandbox.enabled = false;
        model = "sonnet";
        includeCoAuthoredBy = false;
      };

      # User memory, read from $CLAUDE_CONFIG_DIR/CLAUDE.md. Carries the
      # companion prompt on top of the usual global instructions, which is
      # this backend's equivalent of maki's register_prompt_hint.
      claudeCompanionMemory = pkgs.writeText "claude-companion-CLAUDE.md" ''
        ${builtins.readFile dotfiles.config."claude/CLAUDE.md"}
        ${builtins.readFile companionPrompt}
      '';

      # claude-agent-acp spawns the `claude` CLI, and user-wide MCP servers
      # reach that CLI only through a --plugin-dir. home-manager builds such a
      # plugin from programs.mcp.servers, but hangs it on
      # programs.claude-code.finalPackage alongside the nono-packs plugin,
      # whose hooks would try to re-enter nono from inside this sandbox. So the
      # companion gets its own plugin, carrying the MCP registry and nothing
      # else. kagi is added the same way maki's mcp.toml adds it.
      claudeCompanionMcpPlugin =
        let
          # The same transform home-manager's claude-code module applies, so
          # the companion's servers are shaped exactly like the normal
          # claude's -- it is what drops the null url/enabled defaults.
          servers =
            lib.mapAttrs (
              name: server:
              lib.hm.mcp.transformMcpServer {
                inherit server;
                extraTransforms = [
                  lib.hm.mcp.addType
                  (lib.hm.mcp.wrapEnvFilesCommand { inherit pkgs name; })
                ];
              }
            ) config.programs.mcp.servers
            // {
              kagi = {
                type = "http";
                url = "http://127.0.0.1:8081/mcp";
              };
            };
        in
        pkgs.runCommand "claude-companion-mcp-plugin" { } ''
          install -Dm644 ${
            jsonFormat.generate "plugin.json" { name = "claude-companion"; }
          } $out/.claude-plugin/plugin.json
          install -Dm644 ${jsonFormat.generate "mcp.json" { mcpServers = servers; }} $out/.mcp.json
        '';

      claudeCompanionCli = pkgs.writeShellScriptBin "claude-companion-cli" ''
        exec ${lib.getExe claude-code} --plugin-dir ${claudeCompanionMcpPlugin} "''${@}"
      '';

      claudeCompanion = pkgs.writeShellScriptBin "claude-companion" ''
        export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"
        ${resolveGhToken}

        export TMPDIR="/tmp/claude-companion-$UID"
        mkdir -p "$TMPDIR"

        # The adapter's own wrapper points this at the plain claude-code
        # package, which would carry no MCP servers.
        export CLAUDE_CODE_EXECUTABLE="${lib.getExe claudeCompanionCli}"

        # Unlike maki's XDG_CONFIG_HOME this can be exported outside the
        # sandbox: nono resolves its own profiles through XDG_CONFIG_HOME,
        # which CLAUDE_CONFIG_DIR leaves alone. So no inner script is needed.
        export CLAUDE_CONFIG_DIR="$HOME/${claudeStateDir}"

        # `acp' runs the adapter agent-shell drives; --silent keeps nono's
        # banner out of its stdio stream. Anything else -- no arguments
        # included -- is the Claude Code TUI on the same config and sandbox,
        # so the deny rules in settings.json are what hold there (the
        # adapter's `disallowedTools' are an Emacs-side setting).
        if [ "''${1:-}" = "acp" ]; then
          shift
          exec ${nonoExe} --silent ${wrapArgs "claude-companion"} \
            ${lib.getExe claude-agent-acp} "''${@}"
        fi

        exec ${nonoExe} ${wrapArgs "claude-companion"} \
          ${lib.getExe claudeCompanionCli} "''${@}"
      '';
    in
    {
      home.packages = [
        claudeCompanion
      ];

      home.file."${claudeStateDir}/settings.json".source = claudeCompanionSettings;
      home.file."${claudeStateDir}/CLAUDE.md".source = claudeCompanionMemory;
      home.file."${claudeStateDir}/skills".source = ../../config/claude/skills;
    };
}
