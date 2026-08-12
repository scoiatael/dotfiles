{ inputs, ... }:
{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  den.aspects.claude.homeManager =
    {
      pkgs,
      ...
    }:
    {
      home.packages = (
        with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
        [
          (claude-code.overrideAttrs { disableTelemetry = true; })
        ]
      );
    };
}
