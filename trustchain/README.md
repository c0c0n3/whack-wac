Trust Chain Proposal
--------------------
> Tryna get funded for our WAC machine idea.

To build the proposal with Nix

```bash
$ cd nix
$ nix build .#proposal
$ open result/trustchain-proposal.pdf
```

To fool around with LaTeX at the CLI

```bash
$ cd nix
$ nix shell
$ cd ../trustchain/tex
$ ./build.sh
$ open main.pdf
```

To run the Haskell scripts

```bash
$ cd nix
$ nix shell
$ cd ../trustchain/haskell
$ ghc --run tree-path.hs
$ ghc --run policy-edsl.hs
```
