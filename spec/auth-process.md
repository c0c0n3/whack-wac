Authorisation process
---------------------
> Who's in and who's out.

How to give an HTTP request permission to go ahead? The server first
figures out which ACL resource protects the resource the HTTP request
targets. Then it evaluates the authorisation rules in that ACL resource
to see what to do. If any of the rules grant access, then the server
fulfills the request otherwise it denies access—and hacks the client
to pieces. The gory details are in section 5 of the WAC spec. Here
we'll build a much less entertaining algebraic model of the most
important bits of section 5 (Authorization Process) and 3.1 (ACL
Resource Discovery).


### ACL resource lookup

The server arranges information resources and ACL resources that
protect them in two hierarchies. WAC (and Solid) seem to imply those
hierarchies are trees. A server keeps an ACL resource tree $ACLResTree$
alongside the resource tree $ResTree$ which makes up the data the
server manages and that the ACL resources protect. Neither tree is
empty. Now a tree is the same as a poset with a terminal object.
Each child-parent edge is a morphism $child \rightarrow parent$ and
the tree's root node is then terminal.

When an HTTP request comes in, the server looks up the ACL resource
that protects the resource the request targets. This lookup procedure
is a functor $aclResOf : ResTree \rightarrow ACLResTree$. So each
path $t_0 \rightarrow t_1 \rightarrow \ldots \rightarrow t_m$ in $ResTree$
goes to a path $u_0 \rightarrow u_1 \rightarrow \ldots \rightarrow u_n$
in $ACLResTree$ with $n \leq m$. Then if $r$ is the resource the
incoming HTTP request targets, $aclResOf(r)$ is its corresponding
ACL resource.

You can see an example $aclResOf$ functor in the top part of the
diagram below. Colours hint how the functor maps resource nodes to
ACL resource nodes: $r_0$, $r_1$, $r_3$, $r_6$ and $r_8$ get mapped,
respectively, to $p_0$, $p_1$, $p_2$, $p_3$ and $p_4$. Since paths
must go to paths, $r_4$ and $r_5$ are forced to map to $p_1$. Ditto
for $r_7$ that must go to $p_3$. Finally, $r_2$ could either map to
$p_0$ or $p_3$, but in our example $aclResOf(r_2) = p_0$.

![ACL resource lookup.][dia.acl]

Functoriality captures WAC's idea of ACL resource inheritance. For
example, $r_7$ "inherits" $r_6$'s ACL resource, $p_3$, because the
functor preserves paths, so out of necessity $aclResOf(r_7) = p_3$.
A more direct way of seeing that is to merge $ResTree$ and $ACLResTree$
into a single tree (poset) as in the bottom part of the diagram.
Nodes are pairs $(r, m)$ where $r$ is a resource and $m$ is either
an ACL resource or the "nothing" placeholder $\bot$ if $r$ inherits
its ACL resource from an ancestor node. So given a node $(r_k, m)$,
the node containing its ACL resource is the least node $(r, x)$
that's equal or greater than $(r_k, m)$ and has $x \neq \bot$.
(This works because there's always an ancestor node with an associated
ACL resource—WAC says the root must have an associated ACL resource.)
Informally: $r_k$'s ACL resource is $m$ if $m \neq \bot$; otherwise
walk your way up to the root, stopping at the first node $(r, x)$
along the way where $x \neq \bot$; $x$ is $r_k$'s ACL resource.

The process of discovering ACL resources is the same. If the server
wants to tell the client which ACL resource protects the target resource
$r$ of an HTTP request, it computes $p = aclResOf(r)$ and puts $p$'s
URI in the `Link` response header.

The catch? Well, the whole WAC discussion about effective ACL resources
and ACL resource discovery boils down to the simple statement that
there's a functor  $aclResOf : ResTree \rightarrow ACLResTree$. Neat.


### ACL evaluation

After looking up the ACL resource, the server has to decide whether
to grant or deny access. The decision is based on the evaluation of
the authorisation rules that make up the ACL resource associated to
the resource $r$ the incoming HTTP request targets. If there's a rule
$p$ such that the request's user is among $p$'s subjects and $r$ is
part of $p$'s objects and the request's method is one of $p$'s modes,
then the server grants access. Otherwise it denies access.

We model this evaluation process as a function
$ev : Request \times ACLResource \rightarrow Bool$, where $Request$
is the set of all HTTP requests. Out of convenience we define functions
$res: Request \rightarrow Resource$ to extract the resource an HTTP
request targets, $user: Request \rightarrow Agent$ to extract the
request's agent and $method: Request \rightarrow Method$ to extract
the request's HTTP method. With these definitions, evaluation is
defined as shown in the diagram below.

![Authorisation evaluation.][dia.eval]

The whole authorisation process from ACL resource lookup to evaluation
is just a function pipeline as in the left hand side of the diagram.
That is, the function you get by the following composition

```math
ev \circ (id \times aclResOf) \circ \langle id, res \rangle =
ev \circ \langle id, aclResOf \circ res \rangle
```

where $id$ is the identity on $Request$,
$\langle f, g \rangle(x) = (f(x), g(x))$ and
$(f \times g)(x, y) = (f(x), g(y))$.




[dia.acl]: ./effective-acl.png
[dia.eval]: ./auth-eval.png
