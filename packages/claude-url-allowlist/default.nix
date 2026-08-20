{ writers, babashka, lib }:

(writers.writeBabashkaBin "claude-url-allowlist" { } (builtins.readFile ./main.clj)).overrideAttrs
  (old: {
    # makeBinWriter's buildCommand does not eval $checkPhase, so append the
    # test run to the build itself.
    buildCommand = old.buildCommand + ''
      HOME="$TMPDIR" MAIN=${./main.clj} BIN="$out/bin/claude-url-allowlist" \
        ${lib.getExe babashka} ${./test.clj}
    '';
  })
