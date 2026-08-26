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
      pass = lib.getExe pkgs.pass;

      makiWrapped = pkgs.writeShellScriptBin "maki" ''
        export ANTHROPIC_API_KEY="$(${pass} anthropic-com-api-token)"
        exec ${maki}/bin/maki "''${@}"
      '';
    in
    {
      home.packages = [ makiWrapped ];

      home.file.".config/maki/mcp.toml".source = tomlFormat.generate "maki-mcp" {
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
      home.file.".config/maki/init.lua".source = dotfiles.config."maki/init.lua";
      programs.git.ignores = lib.mkAfter [ "/.maki" ];
    };
}
