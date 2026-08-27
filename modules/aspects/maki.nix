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
      pkgs,
      lib,
      ...
    }:
    let
      tomlFormat = pkgs.formats.toml { };

      # [[id:ecb2f488-6fe0-450b-9fd4-34b1b1686587][llm-maki]]
      maki = inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default;
      nono = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.nono;
      pass = lib.getExe pkgs.pass;

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

      # An XDG_CONFIG_HOME for the companion, so it gets its own init.lua
      # without the normal `maki` seeing any of it. maki resolves its config
      # dir through etcetera's base strategy, which honours XDG_CONFIG_HOME.
      companionConfigHome = pkgs.linkFarm "maki-companion-config" [
        {
          name = "maki/init.lua";
          path = dotfiles.config."maki/companion-init.lua";
        }
        {
          name = "maki/mcp.toml";
          path = mcpToml;
        }
      ];

      # Runs inside the sandbox. XDG_CONFIG_HOME is set here rather than in
      # the outer launcher because nono resolves its own profiles through
      # XDG_CONFIG_HOME too -- exporting it before invoking nono makes it
      # fail with "Profile not found: maki-companion".
      makiCompanionInner = pkgs.writeShellScriptBin "maki-companion-inner" ''
        export XDG_CONFIG_HOME="${companionConfigHome}"
        exec ${maki}/bin/maki acp
      '';

      # Read-only ACP server for the Emacs companion agent (agent-shell).
      # Resolves the API key before entering the sandbox: nono's
      # deny_credentials and deny_keychains_macos groups block gpg inside it.
      makiCompanion = pkgs.writeShellScriptBin "maki-companion" ''
        export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"

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
    in
    {
      home.packages = [
        makiWrapped
        makiCompanion
      ];

      home.file.".config/maki/mcp.toml".source = mcpToml;
      home.file.".config/maki/init.lua".source = dotfiles.config."maki/init.lua";
      programs.git.ignores = lib.mkAfter [ "/.maki" ];
    };
}
