Whack WAC
---------
> messing around with the wac spec…

* [A maths model of the WAC spec][model]. Or, the spec re-spec'ed at
  a slightly higher level of abstraction and accuracy.
* [WAC machine][code]. Encoding of the WAC maths model in Haskell.
  Basically an executable spec.
* [Proposal][proposal]. Decentralised, provably secure and consistent
  Web Access Control. Can it be a thing?

#### Building/running stuff
You'll need to install Nix to build LaTeX docs and run Haskell code
in the `proto` and `trustchain/haskell` dirs. Here's how to install
and configure Nix

```bash
$ sh <(curl -L https://nixos.org/nix/install) --daemon
$ mkdir -p ~/.config/nix
$ echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

Our Nix shell has all the tools you need prepackaged for you. To
start it

```bash
$ cd nix
$ nix shell
```

Now you can e.g. run Haskell code like this

```bash
$ ghc --run path/to/file.hs
```




[code]: ./proto/
[model]: ./spec/README.md
[proposal]: ./trustchain/README.md
