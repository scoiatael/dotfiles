{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
stdenv.mkDerivation {
  pname = "ternimal";
  version = "0.1.0";

  src = fetchTarball {
    url =
      "https://github.com/p-e-w/ternimal/archive/e7953b4f80e514899e0920f0e36bb3141b685122.tar.gz";
    sha256 = "0wx39hvd6d0dcpnaiwyv1m13kpk5svmr9a48zm1gkdnj3g0nx35q";
  };

  nativeBuildInputs = [ libiconv ];
  buildInputs = [ rustc ];

  buildPhase = ''
    rustc -O ternimal.rs
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv ternimal $out/bin
  '';
}
