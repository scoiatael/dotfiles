// Strict replacement for TypeNix's `Nixpkgs`, which ends in `[key: string]: any`.
// Every attribute a package uses must be declared here; a typo is a type error.

interface Derivation {
  pname?: string;
  version?: string;
  src?: Derivation;
  meta: Record<string, any>;
  override: (args: Record<string, any>) => Derivation;
  overrideAttrs: (f: Record<string, any> | ((old: Record<string, any>) => Record<string, any>)) => Derivation;
}

type FetchFromGitHubArgs = {
  owner: string;
  repo: string;
} & ({ hash: string } | { sha256: string }) &
  ({ rev: string; tag?: never } | { tag: string; rev?: never });

// Builders accept arbitrary extra attributes, which end up as env vars of the build.
type BuilderArgs = {
  pname?: string;
  name?: string;
  version?: string;
  src?: Derivation | Path | string;
  meta?: Record<string, any>;
  [key: string]: any;
};
type Builder<A = {}> = (args: (BuilderArgs & A) | ((finalAttrs: any) => BuilderArgs & A)) => Derivation;

type Python = Derivation & {
  withPackages: (f: (ps: PythonPkgs) => Derivation[]) => Derivation;
};

type PythonPkgs = {
  buildPythonApplication: Builder;
  buildPythonPackage: Builder;
  callPackage: (fn: string, args: Record<string, any>) => Derivation;
  click: Derivation;
  llm: Derivation;
  markitdown: Derivation;
  poetry_core: Derivation;
  pytestCheckHook: Derivation;
  setuptools: Derivation;
};

// python3Packages.callPackage resolves arguments from both the Python set and top-level pkgs.
type PythonCallPackageArgs = PythonPkgs & Pick<Pkgs, "fetchFromGitHub">;

type Pkgs = ByNamePackages & {
  lib: Lib;
  stdenv: Stdenv;

  fetchFromGitHub: (args: FetchFromGitHubArgs) => Derivation;
  fetchgit: (args: { url: string; hash: string; rev?: string }) => Derivation;
  fetchurl: (args: { url: string; hash: string }) => Derivation;
  fetchPypi: (args: { pname: string; version: string; [key: string]: any } & ({ hash: string } | { sha256: string })) => Derivation;

  buildGoModule: Builder<{ vendorHash: string | null }>;
  rustPlatform: {
    buildRustPackage: Builder;
    importCargoLock: (args: { lockFile: Path | string; [key: string]: any }) => Derivation;
  };
  tmuxPlugins: { mkTmuxPlugin: Builder<{ pluginName: string }> };
  mkShellNoCC: Builder;
  callPackage: (fn: any, args: Record<string, any>) => any;

  writers: { writeBabashkaBin: (name: string, opts: Record<string, any>, script: string) => Derivation };
  writeShellScriptBin: (name: string, text: string) => Derivation;
  writeShellApplication: (args: { name: string; text: string; runtimeInputs?: Derivation[]; [key: string]: any }) => Derivation;

  python3: Python;
  python312: Python;
  python3Packages: PythonPkgs;

  graalvmPackages: { graalvm_ce: Derivation };

  autoPatchelfHook: Derivation;
  autoreconfHook: Derivation;
  babashka: Derivation;
  gcc: Derivation;
  gexiv2_0_16: Derivation;
  gnumake: Derivation;
  gnupg: Derivation;
  libpulseaudio: Derivation;
  magika: Derivation;
  openssl: Derivation;
  pass: Derivation;
  wrapGAppsHook4: Derivation;

  // TypeNix rewrites `-` to `_` in Nix identifiers, so hyphenated attrs are only reachable under these names.
  alsa_lib: Derivation;
  gtk_layer_shell: Derivation;
  linux_pam: Derivation;
  pkg_config: Derivation;
  wl_clipboard: Derivation;
  zsh_fzf_tab: Derivation;
};
