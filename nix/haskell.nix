{pkgs, ...}: let
  http-api-data = pkgs.callPackage ./http-api-data.nix {};
  attoparsec-iso8601 = pkgs.callPackage ./attoparsec-iso8601.nix {};
in
  pkgs.haskell.packages.ghc924.extend (final: prev: {
    # "http-api-data" =
    #   final.callCabal2nix
    #   "http-api-data"
    #   http-api-data
    #   {};
    #
    # "attoparsec-iso8601" =
    #   final.callCabal2nix
    #   "attoparsec-iso8601"
    #   "${attoparsec-iso8601}/attoparsec-iso8601"
    #   {};
    #
    # "aeson" =
    #   final.callCabal2nix
    #   "aeson"
    #   attoparsec-iso8601
    #   {};
  })
