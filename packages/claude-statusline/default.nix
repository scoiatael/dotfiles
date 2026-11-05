{
  writers,
  babashka,
  lib,
}:

(writers.writeBabashkaBin "claude-statusline" { } (builtins.readFile ./main.clj)).overrideAttrs
  (old: {
    # makeBinWriter's buildCommand does not eval $checkPhase, so append the
    # test run to the build itself.
    buildCommand = old.buildCommand + ''
      HOME="$TMPDIR" ${lib.getExe babashka} ${./.}/test.clj
    '';
  })
