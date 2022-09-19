{ pkgs ? import (fetchTarball {
  name = "nixos-unstable-2022-08-09";
  url =
    "https://github.com/NixOS/nixpkgs/archive/12363fb6d89859a37cd7e27f85288599f13e49d9.tar.gz";
  sha256 = "sha256:1rjf0difaznpaq8aal3hc8dzk04x3pwz7pr8ak9svjnn3ysqwl89";
}) { } }:
pkgs.stdenv.mkDerivation rec {
  pname = "emacs-mac";
  version = "0.1.0";

  src = pkgs.fetchgit {
    url = "https://bitbucket.org/mituharu/emacs-mac.git";
    rev = "3ff676c2f98cb6c47fecb37f31a589a910dd3876";
    sha256 = "sha256-v353wmeRKLYsktJWsCHkFWLynPcWpDlu8FdUNEhYVcI=";
  };

  buildInputs = with pkgs; [
    zlib
    ncurses
    libxml2
    gnutls
    texinfo
    gettext
    jansson
  ];
  nativeBuildInputs = with pkgs; [
    pkg-config
    makeWrapper
    autoconf
    automake

    libgccjit
    zlib
    libjpeg
    libungif
    libtiff
    ncurses
  ];

  pathPhase = ''
    echo 'foo'
          exit 1
            bash autogen.sh
  '';

  configureFlags = [
    "LDFLAGS=-L${pkgs.ncurses.out}/lib-l${pkgs.zlib.out}/include"
    "--disable-build-details" # for a (more) reproducible build
    "--with-modules"
    "--with-mac-metal"
    "--with-json"
    "--with-zlib"
    "--with-native-compilation=no"
    "--with-x-toolkit=no"
    "--enable-mac-app=$$out/Applications"
  ];

  installTargets = [ "tags" "install" ];
  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp
    cp ./site-start.el $out/share/emacs/site-lisp/site-start.el
    $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
    siteVersionDir=`ls $out/share/emacs | grep -v site-lisp | head -n 1`
    rm -r $out/share/emacs/$siteVersionDir/site-lisp

    mkdir -p $out/Applications
    mv mac/Emacs.app $out/Applications

    ln -snf $out/lib/emacs/*/native-lisp $out/Applications/Emacs.app/Contents/native-lisp

    echo "Generating native-compiled trampolines..."
    # precompile trampolines in parallel, but avoid spawning one process per trampoline.
    # 1000 is a rough lower bound on the number of trampolines compiled.
    $out/bin/emacs --batch --eval "(mapatoms (lambda (s) \
      (when (subr-primitive-p (symbol-function s)) (print s))))" \
      | xargs -n $((1000/NIX_BUILD_CORES + 1)) -P $NIX_BUILD_CORES \
        $out/bin/emacs --batch -l comp --eval "(while argv \
          (comp-trampoline-compile (intern (pop argv))))"
    mkdir -p $out/share/emacs/native-lisp
    $out/bin/emacs --batch \
      --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp\")" \
      -f batch-native-compile $out/share/emacs/site-lisp/site-start.el
  '';

  NATIVE_FULL_AOT = "1";
  LIBRARY_PATH = "${pkgs.stdenv.cc.libc}/lib";

  LDFLAGS = "-L${pkgs.libgccjit.out}/lib/libgccjit.so";
}
