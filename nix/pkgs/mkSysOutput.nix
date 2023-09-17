#
# Function to generate the Flake output for a given system.
#
{ # System label---e.g. "x86_64-linux", "x86_64-darwin", etc.
  system,
  # The Nix package set for the input system, possibly with
  # overlays from other Flakes bolted on.
  sysPkgs,
  ...
}:
{
  defaultPackage.${system} = with sysPkgs;
  let
    haskell = haskellPackages.ghcWithPackages (ps: []);    # (1)
    tex = nixie.packages.${system}.tex;
  in buildEnv {
    name = "whack-wac-shell";
    paths = [ haskell tex ];
  };
}
# NOTE
# ----
# 1. Could use Haskell env from Nixie. Bring it in if really needed
# though, since it comes with lots of packages.
