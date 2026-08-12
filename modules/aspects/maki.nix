{
  inputs,
  dotfiles,
  ...
}:

{
  flake-file.inputs.maki.url = "github:tontinton/maki";

  den.aspects.maki.homeManager =
    {
      pkgs,
      ...
    }:
    let
      tomlFormat = pkgs.formats.toml { };
    in
    {
      home.packages = [
        inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default # [[id:ecb2f488-6fe0-450b-9fd4-34b1b1686587][llm-maki]]
      ];

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
    };
}
