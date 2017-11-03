{ }:
let
    pkgs = import <nixpkgs> { };
in
    pkgs.stdenv.mkDerivation {
        name = "hello-nix";
        buildInputs = [ pkgs.pkgconfig pkgs.openssl ];
    }
