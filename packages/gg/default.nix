{ lib, stdenv, darwin, fetchNpmDeps, nodePackages, fetchFromGitHub, gnused, perl
, iconv, openssl, pkg-config, rustPlatform, webkitgtk, buildNpmPackage }:

let
  pname = "gg";
  version = "unstable-0.20.0";

  src = fetchFromGitHub {
    owner = "gulbanana";
    repo = pname;
    rev = "3fe15f8a4bea370c0c9e2d7e95d453a73395013b";
    sha256 = "sha256-xOi/AUlH0FeenTXz3hsDYixCEl+yr22PGy6Ow4TKxY0=";
  };

  frontend-build = buildNpmPackage {
    inherit version src;
    pname = "gg-ui";

    postPatch = "";

    npmDepsHash = "sha256-oHBFuX65D/FgnGa03jjpIKAdH8Q4c2NrpD64bhfe720=";

    postBuild = ''
      mkdir -p $out
      cp -r dist $out/
      cp -r node_modules $out/
    '';

    dontInstall = true;
  };

in rustPlatform.buildRustPackage {
  inherit version src pname;

  sourceRoot = "${src.name}/src-tauri";

  cargoLock = { lockFile = ./Cargo.lock; };

  postConfigure = ''
    # Problem 1: duplication of 'cargo-vendor-dir' caused by relative path in
    #  https://github.com/NixOS/nixpkgs/blob/7f13fd4e7960fe2cefaba1048ea1885014311635/pkgs/build-support/rust/import-cargo-lock.nix#L234
    CARGO_VENDOR_DIR=$(realpath ../../cargo-vendor-dir)
    substituteInPlace ../../.cargo/config --replace-fail "cargo-vendor-dir" "$CARGO_VENDOR_DIR"
    substituteInPlace ../../cargo-vendor-dir/.cargo/config --replace-fail "cargo-vendor-dir" "$CARGO_VENDOR_DIR"

    # Problem 2: locked dependencies of jj-cli missing in main Cargo.lock
    rm $CARGO_VENDOR_DIR/*/Cargo.lock
  '';

  postPatch = ''
    # Problem 3: "optional" dependencies of jj-cli are missing in main Cargo.lock but fail build process
    # Needed to generate a new one with it and replace upstream one
    ln -sf ${./Cargo.lock} ./Cargo.lock
    ln -sf ${./Cargo.toml} ./Cargo.toml

    # Tell Tauri where to expect frontend files
    mkdir -p ui-build
    ls  ${frontend-build}/
    cp -R ${frontend-build}/dist/* ui-build/
    substituteInPlace tauri.conf.json \
       --replace-fail '"frontendDist": "../dist"' '"frontendDist": "ui-build"' \
       --replace-fail '"beforeBuildCommand": "npm run build",' '"beforeBuildCommand": "",'
  '';

  buildPhase = ''
    cd ../
    ${frontend-build}/node_modules/.bin/tauri build --bundles app
    echo "buildPhase done"
  '';

  installPhase = ''
    cp -R src-tauri/target $out/
  '';

  nativeBuildInputs = [
    pkg-config
    # for openssl-sys
    perl
  ];

  buildInputs = [ openssl iconv ] ++
    # Problem 4:
    # Darwin build requires:
    #   - Security/SecureTransport.h because of libgit2
    #   - AppKit, WebKit to link main executable
    (lib.lists.optionals (stdenv.isDarwin)
      (with darwin.apple_sdk.frameworks; [ Security AppKit WebKit ]));

  doCheck = false;
}
