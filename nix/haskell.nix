{pkgs, ...}:
pkgs.haskell.packages.ghc924.extend (final: prev: {
  # "aeson" =
  #   final.callCabal2nix
  #   "aeson"
  #   attoparsec-iso8601
  #   {};
})
