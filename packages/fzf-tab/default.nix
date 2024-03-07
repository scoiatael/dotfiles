{ pkgs ? import <nixpkgs> { }, ... }:

# NOTE; looks like there's a problem with nix case hack in here.
# on macOS src is instead named 'src\~nix\~case\~hack\~1' due to conflict with Src
let inherit (pkgs) zsh;
in pkgs.zsh-fzf-tab.overrideAttrs {
  configurePhase = ''
    runHook preConfigure

    pushd modules

    tar -xf ${zsh.src}
    ln -s $(pwd)/src*/fzftab.c zsh-${zsh.version}/Src/Modules/
    ln -s $(pwd)/src*/fzftab.mdd zsh-${zsh.version}/Src/Modules/

    pushd zsh-${zsh.version}

    ls -l Src/Modules

    if [[ ! -f ./configure ]]; then
      ./Util/preconfig
    fi
    if [[ ! -f ./Makefile ]]; then
      ./configure --disable-gdbm --without-tcsetpgrp
    fi

    popd
    popd

    runHook postConfigure
  '';
}
