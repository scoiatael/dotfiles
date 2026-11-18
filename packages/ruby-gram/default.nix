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
  id = "ruby";
  version = "0.16.22";

  src = pkgs.fetchFromGitHub {
    owner = "zed-extensions";
    repo = "ruby";
    tag = "v${finalAttrs.version}";
    hash = "sha256-sRGt8pRNwOZVNry3iEklJqSNGQCl0aYhMn2jJt0aaGA=";
  };

  cargoHash = "sha256-go9NuoT1yKjXR3HKr60/xpbAVCjdmeMd+HWlWd3GVIo=";

  # Commits pinned in the extension's extension.toml.
  grammars = {
    ruby = pkgs.fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-ruby";
      rev = "71bd32fb7607035768799732addba884a37a6210";
      hash = "sha256-iu3MVJl0Qr/Ba+aOttmEzMiVY6EouGi5wGOx5ofROzA=";
    };
    embedded_template = pkgs.fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-embedded-template";
      rev = "c70c1de07dedd532089c0c90835c8ed9fa694f5c";
      hash = "sha256-nBQain0Lc21jOgQFfvkyq615ZmT8qdMxtqIoUcOcO3A=";
    };
    rbs = pkgs.fetchFromGitHub {
      owner = "joker1007";
      repo = "tree-sitter-rbs";
      rev = "5282e2f36d4109f5315c1d9486b5b0c2044622bb";
      hash = "sha256-GE5cW1Nf7S8u5vAGcBZBzfM7mC7yXVR/Tp7hlhmkelA=";
    };
  };
})
