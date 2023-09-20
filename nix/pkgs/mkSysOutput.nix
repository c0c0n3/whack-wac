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
    tex = nixie.packages.${system}.tex;                    # (2)
  in buildEnv {
    name = "whack-wac-shell";
    paths = [ haskell tex ];
  };

  packages.${system}.proposal = with sysPkgs;
  let
    mkPdf = nixie.pkgs.${system}.tex.mkPdfs;
  in mkPdf {
    pkgName = "trustchain-proposal";
    srcDir = ../../trustchain/tex;
    targetFiles = [ "main.tex" ];
    extraPkgs = [ "alegreya" "eulervm" "sourcecodepro" ];
  };
}
# NOTE
# ----
# 1. Could use Haskell env from Nixie. Bring it in if really needed
# though, since it comes with lots of packages.
# 2. Adding extra Tex packages. The Nixie default env comes with the
# most used ones, but we can easily add more if need be. See
# - https://github.com/c0c0n3/nixie/wiki/Texie
