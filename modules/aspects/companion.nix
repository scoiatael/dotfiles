{
  inputs,
  dotfiles,
  den,
  ...
}:

{
  # A read-only companion agent driven from Emacs over ACP (agent-shell) and
  # sandboxed by nono. It searches and suggests while the user works; it cannot
  # change their files.
  #
  # Two interchangeable backends, same shape each time: an outer launcher that
  # resolves the API key before entering the sandbox, a config tree that drops
  # the write tools, and the shared prompt in config/companion/prompt.md.
  # `scoiatael/companion-backend' in the doom `scoiatael/llm' module picks
  # which launcher agent-shell spawns, so switching needs no rebuild.
  #
  # [[id:b842d083-9f81-44d4-b3e6-f140c32a8bee][llm-maki-companion]]
  den.aspects.companion.includes = [
    den.aspects.maki
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
      jsonFormat = pkgs.formats.json { };

      maki = inputs.maki.packages.${system}.default;
      nono = inputs.llm-agents.packages.${system}.nono;
      claude-agent-acp = inputs.llm-agents.packages.${system}.claude-agent-acp;
      claude-code = inputs.llm-agents.packages.${system}.claude-code;
      pass = lib.getExe pkgs.pass;
      gh = lib.getExe pkgs.gh;

      # gh keeps its token in the macOS keychain, which the sandbox denies and
      # cannot grant per item, so it is resolved out here like the API key.
      # Prefers a fine-grained read-only PAT; falls back to the keychain token,
      # which carries `repo` write scope -- the sandbox cannot restrain that,
      # since anything gh does is network, not filesystem.
      resolveGhToken = ''
        GH_TOKEN="$(${pass} show github-com-readonly-token 2>/dev/null | head -1)"
        if [ -z "$GH_TOKEN" ]; then
          GH_TOKEN="$(${gh} auth token 2>/dev/null)"
        fi
        [ -n "$GH_TOKEN" ] && export GH_TOKEN || unset GH_TOKEN
      '';

      companionPrompt = dotfiles.config."companion/prompt.md";

      ## maki backend

      # The companion wants exactly the MCP servers the normal maki has, so it
      # reuses the file the `maki' aspect already generates rather than
      # regenerating an identical one.
      makiMcpToml = config.home.file.".config/maki/mcp.toml".source;

      # The prompt is shared with the claude backend, so it is appended here
      # instead of living in the lua. Long-bracket strings end at `]]`, which
      # the prompt must therefore not contain.
      makiCompanionInit = pkgs.writeText "maki-companion-init.lua" ''
        ${builtins.readFile dotfiles.config."maki/companion-init.lua"}
        maki.api.register_prompt_hint({
        	slot = "after_instructions",
        	prompt = "system",
        	content = [[
        ${builtins.readFile companionPrompt}]],
        })
      '';

      # An XDG_CONFIG_HOME for the companion, so it gets its own init.lua
      # without the normal `maki` seeing any of it. maki resolves its config
      # dir through etcetera's base strategy, which honours XDG_CONFIG_HOME.
      makiCompanionConfigHome = pkgs.linkFarm "maki-companion-config" [
        {
          name = "maki/init.lua";
          path = makiCompanionInit;
        }
        {
          name = "maki/mcp.toml";
          path = makiMcpToml;
        }
        {
          name = "maki/AGENTS.md";
          path = dotfiles.config."maki/AGENTS.md";
        }
      ];

      # Runs inside the sandbox. XDG_CONFIG_HOME is set here rather than in
      # the outer launcher because nono resolves its own profiles through
      # XDG_CONFIG_HOME too -- exporting it before invoking nono makes it
      # fail with "Profile not found: maki-companion".
      makiCompanionInner = pkgs.writeShellScriptBin "maki-companion-inner" ''
        export XDG_CONFIG_HOME="${makiCompanionConfigHome}"
        exec ${maki}/bin/maki acp
      '';

      # Resolves the API key before entering the sandbox: nono's
      # deny_credentials and deny_keychains_macos groups block gpg inside it,
      # so this execs the raw binary rather than the pass-wrapping `maki`.
      makiCompanion = pkgs.writeShellScriptBin "maki-companion" ''
        export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"
        ${resolveGhToken}

        # The one writable spot, for one-off scripts and scratch output.
        # Pointing TMPDIR at it keeps tools that use mktemp inside the grant.
        # $UID matches how nono expands it in the profile's filesystem grant.
        export TMPDIR="/tmp/maki-companion-$UID"
        mkdir -p "$TMPDIR"

        # --silent: nono's banner must not reach the ACP stdio stream.
        # --workdir: expands $WORKDIR in the profile, which grants write to
        # the project's .codegraph/ -- SQLite needs to create WAL/journal
        # files beside the DB even to read it.
        exec ${lib.getExe' nono "nono"} --silent wrap \
          --profile maki-companion --allow-cwd --workdir "$PWD" -- \
          ${makiCompanionInner}/bin/maki-companion-inner
      '';

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
        permissions.deny = [
          "Write"
          "Edit"
          "NotebookEdit"
        ];
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
          servers = lib.mapAttrs (
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
          install -Dm644 ${
            jsonFormat.generate "mcp.json" { mcpServers = servers; }
          } $out/.mcp.json
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

        exec ${lib.getExe' nono "nono"} --silent wrap \
          --profile claude-companion --allow-cwd --workdir "$PWD" -- \
          ${lib.getExe claude-agent-acp}
      '';
    in
    {
      home.packages = [
        makiCompanion
        claudeCompanion
      ];

      home.file."${claudeStateDir}/settings.json".source = claudeCompanionSettings;
      home.file."${claudeStateDir}/CLAUDE.md".source = claudeCompanionMemory;
      home.file."${claudeStateDir}/skills".source = ../../config/claude/skills;
    };
}
