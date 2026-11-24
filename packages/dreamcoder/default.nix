# @ts: { pkgs: Pkgs }
{ pkgs, ... }:

let
  pname = "dreamcoder";
  version = "1.0.16";
  baseUrl = "https://www.dreamcoder.ai/downloads/files/public/${version}";
  meta = {
    homepage = "https://www.dreamcoder.ai";
    mainProgram = "dreamcoder";
    platforms = [
      "aarch64-darwin"
      "x86_64-linux"
    ];
  };

  darwin = pkgs.stdenvNoCC.mkDerivation {
    inherit pname version meta;
    src = pkgs.fetchurl {
      url = "${baseUrl}/macos/dreamcoder-${version}-macos-arm64.zip";
      hash = "sha256-MIcNU601jFMhprxUoNCF3aV+6bDEpjNNzhaXhAA21tg=";
    };
    nativeBuildInputs = [ pkgs.unzip ];
    sourceRoot = ".";
    # The bundle is code-signed; fixup would rewrite its binaries and break the signature.
    dontFixup = true;
    installPhase = ''
      # unzip writes the archive's AppleDouble entries as `._*` files, and alembic loads those as migrations.
      find DreamCoder.app -name '._*' -delete
      mkdir -p $out/Applications $out/bin
      cp -R DreamCoder.app $out/Applications/
      ln -s $out/Applications/DreamCoder.app/Contents/MacOS/dreamcoder-server $out/bin/dreamcoder
    '';
  };

  linux = pkgs.stdenv.mkDerivation {
    inherit pname version meta;
    src = pkgs.fetchurl {
      url = "${baseUrl}/linux-deb/dreamcoder-dreamcoder_${version}_amd64.deb";
      hash = "sha256-rdgbInQbcfNEJqdlHESf5GDJeQq0UZDjNy4ZBHLfvq4=";
    };
    nativeBuildInputs = [
      pkgs.dpkg
      pkgs.autoPatchelfHook
    ];
    buildInputs = [
      pkgs.stdenv.cc.cc.lib
      pkgs.zlib
    ];
    installPhase = ''
      mkdir -p $out/bin $out/lib
      cp -R usr/share $out/share
      cp -R usr/lib/dreamcoder/dreamcoder $out/lib/dreamcoder
      ln -s $out/lib/dreamcoder/dreamcoder-server $out/bin/dreamcoder
      substituteInPlace $out/share/applications/ai.dreamcoder.dreamcoder.desktop \
        --replace-fail /usr/lib/dreamcoder/dreamcoder/dreamcoder-server dreamcoder
    '';
  };
in
if pkgs.stdenv.hostPlatform.isDarwin then darwin else linux
