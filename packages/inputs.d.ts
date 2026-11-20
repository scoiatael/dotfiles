// Flake inputs passed to packages via `callPackageWith (pkgs // { inherit inputs; })`.
// Declare each input a package uses; hyphenated names use `_` (see pkgs.d.ts).

type PerSystem<T> = Record<string, T>;

type Inputs = {
  nix_gram_extensions: {
    packages: PerSystem<{ buildGramRustExtension: Builder<{ id: string; cargoHash: string }> }>;
  };
  uv2nix: {
    lib: { workspace: { loadWorkspace: (args: { workspaceRoot: Derivation | Path | string }) => any } };
  };
  pyproject_nix: { build: { packages: any } };
  pyproject_build_systems: { overlays: { wheel: any } };
};
