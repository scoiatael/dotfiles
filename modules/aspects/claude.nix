{ den, inputs, ... }:
{
  den.aspects.claude.includes = [ den.aspects.llm-agents ];

  den.aspects.claude.homeManager =
    {
      pkgs,
      ...
    }:
    {
      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;

        package =
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code.overrideAttrs
            {
              disableTelemetry = true;
            };

        context = ../../config/claude/CLAUDE.md;
        skills = ../../config/claude/skills;

        settings = {
          tui = "default";

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
