Data model
----------
> WAC-a-lingo.

The core data of WAC are authorisation rules and ACL resources. You
use authorisation rules to control access to resources and collect
rules in ACL resources—think access control policies. Let's start
with a little intro about these concepts and then build on this intro
to come up with an algebraic model describing the WAC data model in
terms of functions among sets. This model is basically a conceptual
framework to understand and reason about the most important bits of
sections 1, 3 (except 3.1) and 4 of the WAC spec.

### ACL resource primer

An authorisation rule says who can do what on which resources. The
who is the *access subject* and it's a subset of the set of all agents
as defined by the FOAF ontology. One particular subset is that of all
*authenticated agents*—i.e. agents the server enforcing authorisation
knows the identity of. The what part of the rule, the *access mode*,
is a subset of the ops (methods) the HTTP spec defines to do stuff
on resources—e.g. `GET`, `POST`. Two handy subsets are
$Read = \lbrace GET, HEAD \rbrace$ and
$Write = \lbrace POST, PUT, PATCH, DELETE \rbrace$.
Finally, the rule controls access to a subset of resources, the
*access object*. As you'd expect, resource means "Web resource
identified by a URI", basically the target of an HTTP request—see
RFC7231.

Here's an example. In the graph below there's an authorisation rule
$auth_1$ whose subject is the set of all agents, mode is the Read
set and object the union of a singleton set containing resource $r_1$
and a two-element set containing resources $r_2$ and $r_3$. So this
rule says anyone can do an HTTP `GET` or `HEAD` on $r_1$, $r_2$ or
$r_3$. Similarly authorisation $auth_2$ says agents $u_1$ and $u_2$
can both read and write $r_8$.

![ACL resource example.][dia.example]

Notice WAC uses three different predicates to specify access objects.
Namely, `default`, `accessTo` and `accessToClass`. This distinction
isn't really useful in our conceptual framework where an access object
is a set. In fact, you can think of e.g. `accessTo` as building a set
with just one resource in it. So it's sets all the way down.

You collect authorisation rules in an access control policy to specify
how to protect your resources. (More about it later.) The policy itself
is a resource, called ACL resource. We'll think of an ACL resource
as a set of authorisation rules. For example, in the graph above, $p_1$
is an ACL resource containing rules $auth_1$ and $auth_2$.


### ACL resource model

Let's start with some basic sets. We'll call $Resource$ the set of
Web resources and $Agent$ the set of all agents that can attempt
access to resources. We'll assume the two sets are disjoints but any
element of those sets can be identified by a URI. Finally, $Method$
is the set of all HTTP methods—`GET`, `POST`, etc.

An authorisation rule holds a subset of $Agent$ (the rule's subject),
a subset of $Method$ (mode), and a subset of $Resource$ (object).
So we define the set $AuthRule$ of all authorisation rules as the
cartesian product of the power sets $\mathcal{P}(Agent)$, $\mathcal{P}(Method)$,
and $\mathcal{P}(Resource)$ with projections, respectively, $subject$,
$mode$, and $object$.

An ACL resource is a set of authorisation rules. So the set of all
ACL resources, $ACLResource$ is the power set $\mathcal{P}(AuthRule)$.
Call $rule$ the mono turning an authorisation rule into a singleton
set. Also, every ACL resource is itself a resource, so call $acl$
the inclusion of $ACLResource$ into $Resource$.

Out of convenience we let $AccessMode = \mathcal{P}(Method)$. WAC
specifies (not fully though) a mapping of access modes to HTTP methods.
We don't need this in our conceptual framework because we model an
access mode as a subset of $Method$. As mentioned earlier, $Read$
and $Write$ are two notable elements of $AccessMode$, but you can
pick any subset you like as a mode.

The diagram of sets and functions below sums up what we talked about
so far. (Notation: fish tail arrow = mono; fish hook = inclusion.)
Notice you could think of this diagram as a set-valued functor $D$
and of the graph example we saw earlier as a diagram in the category
of elements of $D$.

![ACL resource model.][dia.model]

As we mentioned earlier, an ACL resource is a kind of resource, so
you can write authorisation rules to protect ACL resources themselves.
For example, in principle you could add a rule $auth_3$ to the ACL
resource $p_1$ from the example we saw earlier that grants agent $u_1$
read and write access to $p_1$ itself as well as $auth_4$ which only
grants $u_3$ read access to $p_1$

```math
auth_3 = (\{ u_1 \}, Read + Write, \{ p_1 \})
```
```math
auth_4 = (\{ u_3 \}, Read, \{ p_1 \})
```
```math
p_1 = \{ auth_1, auth_2, auth_3, auth_4 \}
```

But there's a snag. WAC says

> When the target of the HTTP request is the ACL resource,
> the operation can only be allowed with the acl:Control access mode.

So if you want to do stuff to an ACL resource you've got to have
`acl:Control`. Now  having `acl:Control` boils down to a rule granting
both read and write permissions—i.e., in our conceptual framework,
the rule's mode is the union $Read + Write$. But then any rule with
an ACL resource as object and mode different than `acl:Control` is
useless in practice. (If the rule is there, no access can be granted;
if the rule isn't there no access can be granted either.) This means
$u_3$ will never be able to read $p_1$ because $auth_4$ has no effect.

#### Control mode
I've got my doubts about the practical utility of `acl:Control`. It
looks like an all-or-nothing proposition, whereas in practice you'll
probably want flexible access modes for ACL resources too. For example,
$auth_4$ could be a totally legit rule if $u_3$ was, say, an external
security auditor you'd like to read but not modify ACL resources.
Chances are I'm misinterpreting the spec.

Maybe `acl:Control` is meant as some sort of elevated access mode.
But I couldn't find any explicit mention of anything like that in
the spec. But then again, even if that was the case, what would its
practical utility be? At the end of the day, what's the difference
between saying

* agent `u` has `acl:Control` over ACL resource `p` and
* agent `u` can `GET`, or`POST`, …, or `DELETE` `p`

if in practice `acl:Control` means `GET`, or`POST`, …, or `DELETE`?

Surely, we could factor all this in our conceptual framework nicely
by redefining $AccessMode$ as a poset with $Read \rightarrow Control$
and $Write \rightarrow Control$ arrows and then a functor
$AccessMode \rightarrow \mathcal{P}(Method)$. But is it really worth
it?




[dia.example]: ./data-model-example.png
[dia.model]: ./data-model.png
