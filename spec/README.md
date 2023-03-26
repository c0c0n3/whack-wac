Getting the hang of WAC
-----------------------
> ...slowly but surely!


TODO the idea: alg model of wac, model semantically equiv
  ==> play model game == play wac game, i.e. same outcome

We're going to put together a little algebraic model to understand
the core of the WAC spec. The model isn't a one-to-one mapping of
the spec and doesn't cover everything. It sits at a slightly higher
level of abstraction and uses basic structures and (structure-preserving)
transformations to try putting together a conceptual framework that,
like all maths models, is precise, concise and can be reasoned about
unambiguously, so we can all draw the same conclusions. At least,
that's the end goal.

**Disclaimer**: the model presented here is a work in progress, some
bits may not be accurate!

This is still a work in progress. It reflects my current understanding
of the spec and I believe it's *semantically equivalent* to the core
of sections 1, 3, 4, and 5. Surely, plain English specs can be delightfully
ambiguous and open to interpretation. So the model presented here is
just one possible formalisation of the WAC game. But hopefully one
that comes close to how you're supposed to play that game in practice.
Any misinterpretation of the spec is obviously my fault.

The two sections below cover most of what you'll probably need to
know to have a solid conceptual understanding of WAC.

* [Data model][data-model]. Core WAC terms, relationships and constraints.
* [Authorisation process][auth-process]. How you're supposed to enforce
  access control on resources using WAC policies.

Topics I left out: HTTP headers (6) and spec extension mechanism (7).
But, hey, after all there's no substitute for reading the spec. By
the way, here's a [reading list][refs] you might find useful.

**TODO**: link Haskell code




[auth-process]: ./auth-process.md
[data-model]: ./data-model.md
[refs]: ./refs.md
