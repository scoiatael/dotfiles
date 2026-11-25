# @ts: Pkgs
{ writers, babashka, lib }:

(writers.writeBabashkaBin "gram-approve" { } (builtins.readFile ./main.clj)).overrideAttrs
  (old: {
    # makeBinWriter's buildCommand does not eval $checkPhase, so append the
    # test run to the build itself.
    buildCommand = old.buildCommand + ''
      HOME="$TMPDIR" MAIN=${./main.clj} BIN="$out/bin/gram-approve" \
        ${lib.getExe babashka} ${./test.clj}
    '';
  })
