{
  inputs,
  dotfiles,
  ...
}:

{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";
  flake-file.inputs.maki.url = "github:tontinton/maki";

  den.aspects.maki.homeManager =
    {
      pkgs,
      lib,
      ...
    }:
    let
      tomlFormat = pkgs.formats.toml { };
    in
    {
      home.packages = [
        inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default # [[id:ecb2f488-6fe0-450b-9fd4-34b1b1686587][llm-maki]]
      ]
      ++ (with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
        nono # [[id:b87289c9-f761-49d5-9f24-a99efbb9f402][llm-nono]]
        codegraph # [[id:caabd499-2344-4dd7-a9de-72fe04af0a49][llm-codegraph]]
      ]);

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
      programs.git.ignores = lib.mkAfter (
        map (p: "/.codegraph/${p}") [
          # CodeGraph data files
          # These are local to each machine and should not be committed

          # Database
          "*.db"
          "*.db-wal"
          "*.db-shm"

          # Cache
          "cache/"

          # Logs
          "*.log"

          # Hook markers
          ".dirty"

          # Misc
          "daemon.pid"
          "codegraph.lock"
          "daemon.sock"

          # Yeah...
          ".gitignore"
        ]
      );
    };
}
