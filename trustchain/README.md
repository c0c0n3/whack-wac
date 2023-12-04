Trust Chain Proposal
--------------------
> Tryna get funded for our WAC machine idea.

To build the proposal with Nix

```bash
$ cd nix
$ nix build .#proposal
$ open result/main.pdf
```

To build the presentation do the same but use `.#interview` instead
of `.#proposal`.

To fool around with LaTeX at the CLI

```bash
$ cd nix
$ nix shell
$ cd ../trustchain/tex/proposal
$ ./build.sh
$ open main.pdf

# ditto for the presentation
```

To run the Haskell scripts

```bash
$ cd nix
$ nix shell
$ cd ../trustchain/haskell
$ ghc --run tree-path.hs
$ ghc --run policy-edsl.hs
```
