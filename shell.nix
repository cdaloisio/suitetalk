{ pkgs ? import <nixpkgs> {} }:

with pkgs;

pkgs.stdenv.mkDerivation {
  name = "suitetalkEnv";
  buildInputs = [
    pkgconfig
    zlib
    openssl
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${zlib.out}/lib:$LD_LIBRARY_PATH"
    export PATH=~/.cabal/bin:$PATH
  '';
}
