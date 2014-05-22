---
layout: page
title: Static Interpretation
---
_By Martin Elsman and Mads Tofte_

### Interpretation of Standard ML Modules at Compile time

The MLKit regards the Modules language as a linking language, which
describes how pieces of code are glued together. The dynamic semantics
of Modules in the Definition of Standard ML can be regarded as
describing an interpreter which combines runtime
environments. However, nothing in the dynamic semantics of Modules
depends on runtime values. Therefore, the dynamic semantics can be
used as a specification of how code fragments can be combined.  By
taking this approach, the MLKit is able to perform all module-level
execution during compilation. Neither structures, nor signatures, nor
functors exist at runtime. Conceptually, the modules language is
simply used for putting together pieces of declarations from the Core
Language.

Functors are elaborated when they are declared, as in most ML
compilers. Thus type errors in functors are caught already when the
functor is declared (as opposed to when it is applied). However, when
the MLKit has elaborated a functor declaration, it postpones code
generation for the functor until the functor is applied. Indeed, if a
functor is applied to two different arguments, code for its body will
be generated twice.  We refer to this approach to Module compilation
as static interpretation. Static interpretation has important
advantages:

### Propagation of Compile-Time Information Across Module Boundaries

When the body of a functor is compiled, the actual argument to which
it is applied is known. Thus one can make use of the information the
compiler has about the actual argument when code is generated for the
functor body. Region Information is a case in point. For example,
consider:

    functor F(X: sig
		   type t
		   val f : int -> t
		   val g : t -> int * int
		 end) = 
    struct
       val it = #1(g(f(5)))
    end;

The formal parameter signature for X does not give any region type
schemes for f and g. For example, the signature does not say whether g
creates the pair it returns in a fresh region, or perhaps always
returns some fixed pair which resides in a global
region. Consequently, region inference of the body of F is not
possible: we cannot know whether the pair returned by g can be
deallocated after the first projection has been applied to it.  Now
consider an application of F:

    structure S = F(struct
		      type t = int
		      fun f(n:int) = n
		      fun g(n:int) = (n,n)
		    end)

Region inference of the actual argument to the call of F shows that
the actual g creates a fresh pair. The code generated for the
application will be equivalent to:

    local
      type t = int
      fun f(n:int) = n
      fun g(n:int) = (n,n)
    in
      val it = #1(g(f(5))
    end

Therefore, region inference will determine that the pair created by g
can indeed be reclaimed after the first projection has been applied to
it.

### No Module-Level Code at Runtime

Since modules are interpreted statically, there is no runtime code for
creating structures. Such code can be quite large and it is often only
executed once. Signature matching generates no code either, not even
in the case where signature matching imposes a less polymorphic type
on a value in a structure.  The theory behind all this and a proof of
its correctness may be found in Martin Elsman's ICFP'99 paper [Static
Interpretation of Modules]({{BASE_PATH}}/pdf/icfp99.pdf).