{
  ...
}:
let
  # llm-agents.nix declares these in its nixConfig, where nix ignores them as untrusted.
  numtideCache = {
    nix.settings = {
      extra-substituters = [ "https://cache.numtide.com" ];
      extra-trusted-public-keys = [
        "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      ];
    };
  };
in
{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  den.aspects.llm-agents = {
    darwin = numtideCache;
    nixos = numtideCache;
  };
}
