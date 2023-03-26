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

The server organises information resources and ACL resources that
protect them in two hierarchies. WAC (and Solid) seem to imply those
hierarchies are trees. A server keeps an ACL resource tree $ACLResTree$
alongside the resource tree $ResTree$ which makes up the data the
server manages and that the ACL resources protect. Neither tree is
empty. We'll think of a tree as a poset with a terminal object—the
tree's root node.

When an HTTP request comes in, the server looks up the ACL resource
that protects the resource the request targets. This lookup procedure
is a functor $aclResOf : ResTree \rightarrow ACLResTree$. So each
path $t_0 \rightarrow t_1 \rightarrow \ldots \rightarrow t_m$ in $ResTree$
goes to a path $u_0 \rightarrow u_1 \rightarrow \ldots \rightarrow u_n$
in $ACLResTree$ with $n \leq m$.

![ACL resource lookup.][dia.acl]


### ACL evaluation

![Authorisation evaluation.][dia.eval]




[dia.acl]: ./effective-acl.png
[dia.eval]: ./auth-eval.png
