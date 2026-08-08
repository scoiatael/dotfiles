{
  den.aspects.llama =
    let
      # https://github.com/ggml-org/llama.cpp/tree/master/tools/server#model-presets
      modelsPreset = {
        "Qwen-AgentWorld" = {
          hf-repo = "unsloth/Qwen-AgentWorld-35B-A3B-GGUF";
          hf-file = "Qwen-AgentWorld-35B-A3B-UD-Q4_K_XL.gguf";
          alias = "unsloth/Qwen-AgentWorld";
        };
        "Qwen3-VL" = {
          hf-repo = "Qwen/Qwen3-VL-8B-Instruct-GGUF";
          hf-file = "Qwen3VL-8B-Instruct-Q8_0.gguf";
          alias = "Qwen/Qwen-VL";
        };
        "Qwen3-Coder-Next" = {
          hf-repo = "unsloth/Qwen3-Coder-Next-GGUF";
          hf-file = "Qwen3-Coder-Next-UD-Q4_K_XL.gguf";
          alias = "unsloth/Qwen3-Coder-Next";
        };
        "DeepSeek-V4-Flash" = {
          hf-repo = "unsloth/DeepSeek-V4-Flash-0731-GGUF:UD-Q2_K_XL";
          alias = "DeepSeek-V4-Flash";
        };
      };
    in
    {
      darwin =
        {
          pkgs,
          lib,
          config,
          self',
          ...
        }:
        let
          homeDir = "/private/var/lib/llama";
          flags = [
            "--host"
            "127.0.0.1"
            "--port"
            "8080"
            "--gpu-layers"
            "all"
            "-hf"
            "ggml-org/Qwen3.5-0.8B-GGUF:Q8_0"
            "--tools"
            "all"
            "--webui-mcp-proxy"
            "--gpu-layers"
            "all"
          ];
          serve = pkgs.writers.writeBash "llama-serve" ''
            ${lib.getExe' pkgs.llama-cpp "llama-server"} ${lib.escapeShellArgs flags}
          '';
        in
        {
          launchd.daemons.llama = {
            command = serve;
            serviceConfig = {
              GroupName = "_llama";
              Label = "dev.scoiatael.llama-server";
              RunAtLoad = true;
              StandardOutPath = "${homeDir}/log/llama-server.out";
              StandardErrorPath = "${homeDir}/log/llama-server.err";
              UserName = "_llama";
              WorkingDirectory = homeDir;
            };
          };
          launchd.daemons.kagimcp =
            let
              flags = [
                "--host"
                "127.0.0.1"
                "--port"
                "8081"
                "--http"
                "--cors-origins"
                "127.0.0.1:8080"
              ];
              serve = pkgs.writers.writeBash "kagimcp" ''
                source ${config.sops.secrets.kagimcp-env.path}
                export KAGI_API_KEY
                ${lib.getExe' self'.packages.kagimcp "kagimcp"} ${lib.escapeShellArgs flags}
              '';
            in
            {
              command = serve;
              serviceConfig = {
                GroupName = "_llama";
                Label = "dev.scoiatael.kagimcp";
                RunAtLoad = true;
                StandardOutPath = "${homeDir}/log/kagimcp.out";
                StandardErrorPath = "${homeDir}/log/kagimcp.err";
                UserName = "_llama";
                WorkingDirectory = homeDir;
              };
            };
          users = {
            users._llama = {
              inherit (config.users.groups._llama) gid;
              createHome = true;
              description = "llama service user";
              home = homeDir;
              shell = "/bin/bash";
              uid = lib.mkDefault 800;
            };
            knownUsers = [ "_llama" ];
            groups._llama = {
              gid = lib.mkDefault 800;
              description = "llama service user group";
            };
            knownGroups = [ "_llama" ];
          };
        };
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
            inherit modelsPreset;
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
