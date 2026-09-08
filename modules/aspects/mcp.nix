{
  den,
  inputs,
  ...
}:

{
  den.aspects.mcp.includes = [ den.aspects.llm-agents ];

  den.aspects.mcp.homeManager =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      # nono's profiles live with the binary rather than with the `claude'
      # aspect, because the aspects that run sandboxes (companion) include
      # `mcp' but not `claude'. Packs from outside this repo come in through
      # this option instead of a second xdg.configFile entry: the profiles are
      # one merged directory, so only one module can own that path.
      options.nono.extraPacks = lib.mkOption {
        type = lib.types.attrsOf lib.types.path;
        default = { };
        example = lib.literalExpression ''{ claude = "''${nono-packs}/claude"; }'';
        description = ''
          nono packs to install alongside the profiles in config/nono, as
          profile name -> pack directory. Each pack's policy.json is linked in
          as <name>.json, so profiles can `extends` it.
        '';
      };

      config = {
        home.packages = (
          with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
          [
            nono # [[id:b87289c9-f761-49d5-9f24-a99efbb9f402][llm-nono]]
            agent-browser
            codegraph # [[id:caabd499-2344-4dd7-a9de-72fe04af0a49][llm-codegraph]]
          ]
        );

        xdg.configFile."nono/profiles".source = pkgs.buildEnv {
          name = "nono-profiles";
          paths = [ ../../config/nono ];
          postBuild = lib.concatLines (
            lib.mapAttrsToList (
              name: pack: ''ln -s "${pack}/policy.json" "$out/${name}.json"''
            ) config.nono.extraPacks
          );
        };

        programs.mcp = {
          enable = true;
          servers = {
            agent-browser = {
              command = "agent-browser";
              args = [ "mcp" ];
            };
            codegraph = {
              command = "codegraph";
              args = [
                "serve"
                "--mcp"
              ];
            };
          };
        };

        programs.git.ignores = lib.mkAfter [ "/.codegraph" ];
      };
    };
}
