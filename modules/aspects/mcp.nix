{
  inputs,
  ...
}:

{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  den.aspects.mcp.homeManager =
    {
      pkgs,
      lib,
      ...
    }:
    {
      home.packages = (
        with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
        [
          # nono # [[id:b87289c9-f761-49d5-9f24-a99efbb9f402][llm-nono]]
          codegraph # [[id:caabd499-2344-4dd7-a9de-72fe04af0a49][llm-codegraph]]
        ]
      );

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
