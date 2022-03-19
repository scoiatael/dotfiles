with import <nixpkgs> {};
fastStdenv.mkDerivation {
  name = "python-build";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ zlib openssl readline sqlite libffi ];
}
