{
  inputs,
  system,
  pkgs,
  ...
}:

let
  inherit (inputs.nix-gram-extensions.packages.${system}) buildGramRustExtension;
in
buildGramRustExtension (finalAttrs: {
  id = "prisma";
  version = "0.1.10";

  src = pkgs.fetchFromGitHub {
    owner = "zed-extensions";
    repo = "prisma";
    tag = "v${finalAttrs.version}";
    hash = "sha256-KuKDTBLgreQpQhy4UG/ojK1XwT4MZu0Oxn3EVhm+IiI=";
  };

  cargoHash = "sha256-I1+TUEDPEdpfI4dXi4ZdANBefo3tqLEhEJedy4dTlN8=";

  # Commit pinned in the extension's extension.toml.
  grammars = {
    prisma = pkgs.fetchFromGitHub {
      owner = "victorhqc";
      repo = "tree-sitter-prisma";
      rev = "3556b2c1f20ec9ac91e92d32c43d9d2a0ca3cc49";
      hash = "sha256-VE9HUG0z6oPVlA8no011vwYI2HxufJEuXXnCGbCgI4Q=";
    };
  };
})
