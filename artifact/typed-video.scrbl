#lang scribble/manual

@title{Typed Video}

Typed Video consists of a type checking layer on top of the
@seclink["Video_Implementation"]{untyped Video language implementation}. This
typed variant requires no changes to the untyped language. The Typed Video
repository may be viewed locally at
@filepath{/home/artifact/Desktop/typed-video/} (or
@hyperlink["https://github.com/stchang/typed-video"]{online}) and is structured
as follows:

@itemlist[
 #:style 'compact
 @item{@filepath{examples/}--all of the @seclink["Video_Examples"]{examples from the untyped Video language}, ported to Typed Video.}
 @item{@filepath{tests/}--additional tests for Typed Video, including all examples from the paper}
 @item{@filepath{typed/}--the source code for Typed Video, which adds a type checking layer on top of the untyped Video implementation. In particular @filepath{typed/video.rkt} contains the type rule implementations.}]

@section{Unit-testing library for type checking}

Examples and tests in the Typed Video repository may utilize a special
 unit-testing library for type checking that consists of the following forms.
 @itemlist[
  @; check-type
  @item{@code{check-type}: given an expression and type, the test passes if the expression has a type that is a subtype of the given type.

  Examples:
  @racketblock0[
   @code:comment{Producer of unlimited length}
   (check-type (color "green") : Producer)
   @code:comment{Producer at least 2 frames long}
   (check-type (color "blue" #:length 2) : (Producer 2))
  ]}

  @; check-not-type
  @item{@code{check-not-type}: given an expression and type, the test passes if the expression has a type that is @emph{not} a subtype of the given type.

  Examples:
  @racketblock0[
   @code:comment{not a Producer at least 3 frames long}
   (check-not-type (color "blue" #:length 2) : (Producer 3))
  ]}

  @; typecheck-fail
  @item{@code{typecheck-fail}: given an expression, the test passes if the expression fails to typecheck. Accepts an optional regexp string that must match the type error message.

  Examples:
  @racketblock0[
   @code:comment{cannot have negative length Producer}
   (typecheck-fail (blank -1))
   @code:comment{with additional type error message check}
   (typecheck-fail (blank -1)
    #:with-msg
    "expression has type \\(Producer -1\\), which fails side-condition: \\(>= -1 0\\)")
  ]}
]

@section{Paper examples}

The main Typed Video examples from section 6.2 of the paper may be found in
@filepath{tests/paper-tests.rkt}, namely the @code{add-slides},
@code{add-bookend}, and @code{conference-talk} functions.

@section{Implementation}

Section 6.4 of the paper (Figure 9) presents the essence of the type rule
implementations for lambda and function application, which utilize the "Type
Systems as Macros" framework. The full implementations for those rules and most
others may be viewed in
@filepath{/home/artifact/Desktop/typed-video/typed/video.rkt}. Specifically, the lambda
rule begins at line 583 and function application begins at line 640. Much of
the extra code that is elided from the paper tries to clean up and simplify the
error messages, e.g., removing duplicate constraints and making sure not to
leak implementation details.
