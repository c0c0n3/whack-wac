#!/usr/bin/env bash

# Build PDF from LaTeX source. This script is just for extra
# convenience, use Nix to build instead.

# Stop on error.
# - https://stackoverflow.com/questions/2870992
set -euxo pipefail

# Poor man's build.
# - https://tex.stackexchange.com/questions/259300
lualatex main.tex
biber main.bcf
lualatex main.tex
lualatex main.tex
