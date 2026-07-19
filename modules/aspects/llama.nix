{
  den.aspects.llama = {
    nixos =
      {
        pkgs,
        self',
        config,
        ...
      }:

      {
        services.llama-cpp = {
          enable = true;
          package = pkgs.llama-cpp-rocm;
          extraFlags = [
            "--webui-mcp-proxy"
            "--tools"
            "all"
          ];

          # Takes care of downloading if model not present
          modelsPreset = {
            "Qwen-AgentWorld" = {
              hf-repo = "unsloth/Qwen-AgentWorld-35B-A3B-GGUF";
              hf-file = "Qwen-AgentWorld-35B-A3B-UD-Q4_K_XL.gguf";
              alias = "unsloth/Qwen-AgentWorld";
            };
            "Qwen3-Coder-Next" = {
              hf-repo = "unsloth/Qwen3-Coder-Next-GGUF";
              hf-file = "Qwen3-Coder-Next-UD-Q4_K_XL.gguf";
              alias = "unsloth/Qwen3-Coder-Next";
            };
          };
        };

        systemd.services.kagimcp = {
          description = "Kagi MCP";
          wantedBy = [ "default.target" ];
          path = [ self'.packages.kagimcp ];
          script = ''
            kagimcp --host 127.0.0.1 --port 8081 --cors-origins llama.heron-pollux.ts.net --http
          '';
          serviceConfig = {
            Type = "simple";
            Restart = "on-failure";
            DynamicUser = true;
            EnvironmentFile = [ config.sops.secrets.kagimcp-env.path ];
          };
        };
      };
  };
}
