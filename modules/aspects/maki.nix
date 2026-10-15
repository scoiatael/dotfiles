{
  inputs,
  dotfiles,
  den,
  ...
}:

{
  flake-file.inputs.maki.url = "github:tontinton/maki";

  den.aspects.maki.includes = [
    den.aspects.llm-agents
    den.aspects.llama
    den.aspects.mcp
  ];
  den.aspects.maki.homeManager =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      tomlFormat = pkgs.formats.toml { };
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

      # [[id:ecb2f488-6fe0-450b-9fd4-34b1b1686587][llm-maki]]
      maki = inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default;

      makiWrapped = pkgs.writeShellScriptBin "maki" ''
        export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"
        exec ${maki}/bin/maki "''${@}"
      '';

      mcpToml = tomlFormat.generate "maki-mcp" {
        mcp = {
          kagi = {
            url = "http://127.0.0.1:8081/mcp";
          };
          codegraph = {
            command = [
              "codegraph"
              "serve"
              "--mcp"
            ];
          };
        };
      };

      ## maki companion backend

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
          path = mcpToml;
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
        exec ${maki}/bin/maki "''${@}"
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

        # `acp' is the server agent-shell drives; --silent keeps nono's banner
        # out of its stdio stream. Anything else -- no arguments included --
        # is maki's own TUI in the same sandbox, where the banner is worth
        # seeing.
        if [ "''${1:-}" = "acp" ]; then
          shift
          exec ${nonoExe} --silent ${wrapArgs "maki-companion"} \
            ${makiCompanionInner}/bin/maki-companion-inner acp "''${@}"
        fi

        exec ${nonoExe} ${wrapArgs "maki-companion"} \
          ${makiCompanionInner}/bin/maki-companion-inner "''${@}"
      '';
    in
    {
      home.packages = [
        makiCompanion
        makiWrapped
      ];

      # The read-only Emacs companion reuses this file; see
      # modules/aspects/companion.nix.
      home.file.".config/maki/mcp.toml".source = mcpToml;
      home.file.".config/maki/AGENTS.md".source = dotfiles.config."maki/AGENTS.md";
      home.file.".config/maki/init.lua".source = dotfiles.config."maki/init.lua";
      programs.git.ignores = lib.mkAfter [ "/.maki" ];
    };
}
