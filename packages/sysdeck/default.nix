{ pkgs, lib, ... }:

let
  # The prebuilt binary links libgexiv2.so.2, which only the 0.14 series ships.
  gexiv2_0_14 = pkgs.gexiv2_0_16.overrideAttrs (old: rec {
    version = "0.14.6";
    src = pkgs.fetchurl {
      url = "mirror://gnome/sources/gexiv2/0.14/gexiv2-${version}.tar.xz";
      hash = "sha256-YGwoqq57Hz71yOq+Xn3/18WhyGbSW3Zx+4R/4oenK4s=";
    };
    outputs = [
      "out"
      "dev"
    ];
    mesonFlags = [ "-Dgtk_doc=false" ];
    doCheck = false;
  });
in
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "sysdeck";
  version = "0.1.0";
  src = pkgs.fetchurl {
    url = "https://github.com/mrusme/sysdeck/releases/download/v${finalAttrs.version}/sysdeck-v${finalAttrs.version}-x86_64-linux-gnu.tar.gz";
    hash = "sha256-MoMT8qogHJbjN1/R73FPfcv49IyaeVnsBSWMoK3Sumk=";
  };
  nativeBuildInputs = [
    pkgs.autoPatchelfHook
    pkgs.wrapGAppsHook4
  ];
  buildInputs = with pkgs; [
    gtk4
    gtk4-layer-shell
    gdk-pixbuf
    cairo
    glib
    sqlite
    gexiv2_0_14
    wayland
  ];
  installPhase = ''
    install -Dm755 sysdeck $out/bin/sysdeck
    install -Dm644 config.example.toml $out/share/sysdeck/config.example.toml
  '';
  preFixup = ''
    gappsWrapperArgs+=(--prefix PATH : ${lib.makeBinPath [ pkgs.wl-clipboard ]})
  '';
  meta = {
    homepage = "https://tty.fail/mrus/sysdeck";
    mainProgram = "sysdeck";
    platforms = [ "x86_64-linux" ];
  };
})
