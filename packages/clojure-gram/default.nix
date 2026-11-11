# TODO Add this to https://codeberg.org/niklaskorz/nix-gram-extensions/src/branch/main
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
  id = "clojure";
  version = "2026-07-07";

  src = pkgs.fetchFromGitHub {
    owner = "zed-extensions";
    repo = "clojure";
    rev = "ae31220e9ea9f608bc74135ffd5e88908536c1d6";
    hash = "sha256-hI/jYcfGUlLbYMe9aiyguxXn3xgNYx7Zdtjwf4SQuFw=";
  };

  cargoHash = "sha256-EzxXvxBQr5FW21MRV60q7pC0/rD3w/JEV4H/BlNhF+s=";

  grammars = {
    clojure = pkgs.fetchFromGitHub {
      owner = "clj-commons";
      repo = "tree-sitter-clojure";
      rev = "e43eff80d17cf34852dcd92ca5e6986d23a7040f";
      hash = "sha256-jokekIuuQLx5UtuPs4XAI+euispeFCwSQByVKVelrC4=";
    };
  };
})
