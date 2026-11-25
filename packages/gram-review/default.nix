# @ts: Pkgs
{ writers, babashka, git, lib }:

(writers.writeBabashkaBin ",gram-review" { } (builtins.readFile ./main.clj)).overrideAttrs
  (old: {
    # makeBinWriter's buildCommand does not eval $checkPhase, so append the
    # test run to the build itself.
    buildCommand = old.buildCommand + ''
      HOME="$TMPDIR" PATH="${lib.getBin git}/bin:$PATH" \
        MAIN=${./main.clj} BIN="$out/bin/,gram-review" \
        ${lib.getExe babashka} ${./test.clj}
    '';
  })
