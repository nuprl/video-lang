#lang scribble/sigplan

@title[#:tag "types"]{The Bodyguard}
@(require (except-in scribble/manual cite)
          scriblib/figure scriblib/footnote
          scribble/core
          racket/list "bib.rkt")

What use is a programming language without a dependent type system? Lots of
course, as Video shows. After all, Video is a scripting language, and most
descriptions of a conference video are no longer than a few lines. No real
programmer needs types for such programs. For some, however,
the existence of an untyped language might be inconceivable, and we
therefore whipped together a dependent type system in a single work day. 

After the rationale for such a type system (@secref{video-data}),
we present the essential idea: type checking the length of producers,
transitions (which are conceptually functions on producers), functions on
transitions, and so on (@secref{index}). Then we examine a few
type-checking rules (@secref{type-system}). Finally, we once again
demonstrate the power of Racket's syntax system, which not only modifies the
syntax of a base language but also adds a type system---with more or less the
familiar notation found in papers on fancy type systems
(@secref{type-implementation}). 

@; -----------------------------------------------------------------------------
@section[#:tag "video-data"]{Video Data Types}

Video programs primarily manipulate two types of data: producers and
transitions. A typical Video program slices and combines these values together.
Not surprisingly, programmer errors often involve manipulating producers and
transitions of improper lengths. Such errors are particularly dangerous since
they can often occur at the C level during rendering. For example, the following
piece of code mistakenly tries to extract 16 frames from a producer that is
only 10 frames long:
@;
@racketblock[
(cut-producer (color "green" #:length 10) #:start 5 #:end 20)
@code:comment{CRASH (given producer must have length >= 16)}]
@;
The following second example attempts to use producers that are too short for
the specified transition:
@;
@racketblock[
(playlist (blank #:length 10) (fade-transition #:length 15) (color #:length 10))
@code:comment{CRASH (given producers must have length >= 15)}]
@;
While scripting languages generally rely on dynamic checks to enforce such
conditions, we design a static type system instead.

@section[#:tag "index"]{Length Indexes}

Typed Video adds a lightweight dependent type system to Video where the types
of producers and transitions are indexed by natural number terms
corresponding to their lengths. The rest of the type system resembles a
simplified version of Xi and Pfenning's ATS@cite[ats-pldi].

Such a type system does not impose too much of a burden in our domain of video
editing and works well in practice because Video programmers are already
accustomed to specifying explicit length information in their programs. With
Typed Video, the examples from @secref{video-data} produce static type error
messages. In general, the type system ensures that producer values do not flow
into positions where their length is less than expected.

Typed Video also supports writing functions polymorphic in the lengths of
videos. Continuing the conference video editing example from earlier in the
paper (@secref{overview}), a function @racket[add-slides] for combining the
video of a speaker with their slides might have the following signature:

@racketblock[
(define (add-slides {n} [video : (Producer n)] [slides : (Producer n)] -> (Producer n))
  (multitrack video slides background #:length (producer-length video)))]
@;
This function binds a universally-quantified type variable @racket[n] that
ranges over natural number terms and uses it to specify that the lengths of the
input and output producers must match up.

In addition, a programmer may specify side-conditions involving type
variables. Here is an @racket[add-bookend] function that adds an
opening and ending sequence to a speaker's video:

@racketblock[
(code:comment "Add conference logos to the front and end of a video.")
(define (add-bookend {n} [main-talk : (Producer n)] #:when (>= n 400) -> (Producer (+ n 600)))
  (playlist begin-clip
            @fade-transition[#:length 200]
	    main-talk
	    @fade-transition[#:length 200]
	    end-clip)
  (define begin-clip @image["logo.png" #:length 500])
  (define end-clip @image["logo.png" #:length 500]))]
@;
The @racket[add-bookend] function specifies with a @racket[#:when] keyword that
its input must be a producer of at least 400 frames because it uses two
200-frame transitions. Moreover, the result type says that the output adds 600
frames to the input. Here the additional frames come from the added intro and
outro, minus the transition frames.

Programmer-specified side-conditions may propagate to other
functions. For example, here is a @racket[make-conference-talk]
function (first introduced in @secref{overview}):

@racketblock[
(define (make-conference-talk {n} [video  : (Producer n)]
	                          [slides : (Producer n)]
				  [audio  : (Producer n)]
				  [offset : Int]
				  -> (Producer (+ n 600)))
  @code:comment{...}
  (define p1 (add-slides video slides))
  (define p2 (add-bookend p1))
  @code:comment{...})]
@;
Even though @racket[make-conference-talk] does not specify an explicit
side-condition, it inherits the @racket[(>= n 400)] side-condition from
@racket[add-bookend]. Thus applying @racket[make-conference-talk] to a video
that is not provably longer than 400 frames results in a type error.

@section[#:tag "type-system"]{The Type System}

@(define (mk-txt x) (list "\\texttt{" x "}"))
@(define (inferrule #:name [name ""] . as)
   (define args (map mk-txt as))
   (make-element (make-style (string-append "inferrule[" name "]")
                             '(exact-chars))
                 (cond [(= 1 (length args))
                        (list " }{" (mk-txt (car args)))]
                       [(>= (length args) 2) @; scribble auto flattens?
                        (define sgra (reverse args))
                        (define conclusion (car sgra))
                        (define premises (reverse (add-between (cdr sgra) "\\\\")))
                        (list premises "}{" conclusion)]
                       [else (printf "shouldnt get here, args = ~a\n" args)])))

@(define (noindent)
   (make-element (make-style "noindent" '(exact-chars)) null))

This subsection presents a taste of Typed Video's type system. We omit a
full formal presentation since (1) it is not the main object of study in
this paper, and (2) Typed Video utilizes only existing type system methods.

As previously mentioned, Video programmers already specify explicit video
lengths in their programs and thus it is easy to lift this information to the
type-level. For example, @figure-ref{type-rules} presents, roughly, a few rules
for creating and consuming producers.
The first rule lifts the specified length to the expression's type. In the
absence of a length argument, as in the second rule, the expression has type
@tt{Producer}, which is sugar for @tt{(Producer ∞)}. The third says that if the
given file @tt{f} on disk points to a video of length @tt{n},@note{Obviously,
the soundness of our type system is now contingent on the correctness of this
system call.}  then an expression @tt{(clip f)} has type @tt{(Producer n)}.
The fourth rule shows how producer lengths may be combined. Specifically, a
@racket[playlist] appends producers together and thus their lengths are
summed. If playlists interleave transitions between producers, the lengths of
the transitions are subtracted from the total because each transition results
in an overlapping of producers. A type error is signaled if the computed length
of a producer is negative.

@figure["type-rules" "A few type rules for Typed Video"
@inferrule[#:name "color-n" "e : String"]{(color e \#:length n) : (Producer n)}

@inferrule[#:name "color" "e : String"]{(color e) : Producer}

@inferrule[#:name "clip" "f : File" "|f| = n"]{(clip f) : (Producer n)}

@inferrule[#:name "playlist" "p/t <: (Producer n) \\textrm{or} p/t <: (Transition m)" "..."]{(playlist p/t ...) : (Producer (- (+ n ...) (+ m ...)))}
]

The fourth rule uses Typed Video's subtyping relation. Here is the subtyping
rule for the @tt{Producer} type:

@inferrule["m >= n"]{(Producer m) <: (Producer n)}

@(noindent)Since Typed Video aims to prevent not-enough-frame errors, it is
acceptable to supply a producer that is longer than expected but not shorter.

In addition to requiring non-negative video lengths, Typed Video imposes
additional restrictions on the terms that may appear in a @tt{Producer}
type. For example, application of arbitrary functions is disallowed to prevent
non-termination of type checking; only addition, subtraction, and a few Video
primitives are supported, which simplifies type checking. If the @tt{Producer}
type constructor is applied to an unsupported term, the type defaults to a
@tt{Producer} of infinite length. Similar restrictions are imposed on
side-conditions. Despite these restrictions, Typed Video works well in
practice and can type check all our example programs.

@; -----------------------------------------------------------------------------
@section[#:tag "type-implementation"]{Type Systems as Macros}

The implementation of Typed Video relies on linguistic reuse and thus
 creates a full-fledged programming language. Specifically, it reuses
 Racket's syntax system to implement type checking, following
 @citet[tsam-popl]'s type-systems-as-macros approach. As a result, Typed
 Video is an extension to, rather than a reimplementation of, the untyped Video
 language.

@Figure-ref{type-checking-macros} shows the implementation of two interesting
rules: @racket[λ] and function application. The @racket[require] at the top of
the figure imports and prefixes the identifiers from untyped Video (i.e., the
syntactic extensions from @secref{implementation}), which are used, unmodified,
as the output of the type-checking pass.

@figure["type-checking-macros" "Type checking macros"
@racketblock[(require (prefix-in untyped-video: video)) @code:comment{imports untyped Video identifiers with a prefix}
             (provide λ #%app) @code:comment{exports Typed Video identifiers}]
@codeblock[#:line-numbers 1]{
(define-syntax/typecheck (λ {n ...} ([x : τ] ... #:when C) e) ≫
  [(n ...) ([x ≫ x- : τ] ...) ⊢ e ≫ e- ⇒ τ_out]
  #:with new-Cs (get-captured-Cs e-)
  ---------
  [⊢ (untyped-video:λ (x- ...) e-) ⇒ (∀ (n ...) (→ τ ... τ_out #:when (and C new-Cs)))])}

@codeblock[#:line-numbers 1]{
(define-syntax/typecheck (#%app e_fn e_arg ...) ≫ 
   [⊢ e_fn ≫ e_fn- ⇒ (∀ Xs (→ τ_inX ... τ_outX #:when CX))]
   #:with τs (solve Xs (τ_inX ...) (e_arg ...))
   #:with (τ_in ... τ_out C) (inst τs Xs (τ_inX ... τ_outX CX))
   #:fail-unless (not (false? C)) "failed side-condition"
   #:unless (boolean? C) (add-C C)
   [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
   --------
   [⊢ (untyped-video:#%app e_fn- e_arg- ...) ⇒ τ_out])}
]

We implement our type checker with Turnstile, a Racket DSL introduced by
Chang et al. for creating Typed DSLs. This DSL-generating-DSL uses a concise,
bidirectional type-judgement-like syntax, as seen in
@figure-ref{type-checking-macros}'s @racket[define-syntax/typecheck]
rules. These rules define macros that incorporate type checking as part
of macro processing. Interleaving type checking and syntax elaboration in this
manner not only simplifies implementation of the type system, but also enables
creating true abstractions on top of the host language.

Next we briefly explain each line of the @racket[λ] definition:

@itemlist[#:style 'ordered

@item{The type-checking macro's input must match pattern @racket[(λ {n} ([x :
τ] ... #:when C) e)], which binds pattern variables @racket[n] (the type
variable), @racket[x] (the λ parameters), @racket[τ] (the type annotations),
@racket[C] (a side-condition), and @racket[e] (the lambda body). Pattern
variables may subsequently be used to construct new program fragments, e.g.,
the outputs of the macro. An ellipses pattern matches zero-or-more of its
preceding pattern; any pattern variables in that pattern requires ellipses when
subsequently used.}

@item{Since type checking is interleaved with syntax elaboration, Turnstile
type judgements are elaboration judgements as well. Specifically, a Turnstile
judgement @racket[[ctx ⊢ e ≫ e- ⇒ τ]] is read ``in context @racket[ctx],
@racket[e] elaborates to @racket[e-] and has type @racket[τ]''.

Thus the lambda body @racket[e] elaborates to to @racket[e-], simultaneously
computing its type @racket[τ_out]. This elaboration and type checking occurs in
the context of the free variables. Instead of propagating a type environment,
Turnstile reuses Racket's lexical scoping to implement the type
environment. This re-use greatly enhances the composition of languages, and a
programmer writes only new environment bindings. Specifically, the premise uses
two type envionments, one each for the type variables and lambda parameters,
respectively, where the latter may contain references to the former.}

@item{A @racket[#:with] premise binds additional pattern variables. Here,
elaborating the lambda body @racket[e] may generate additional side-conditions,
@racket[new-Cs], that must be satisfied by the function's inputs. Turnstile
allows specifying propagation of not just types, but arbitrary metadata on the
program tree, and we use this mechanism to compute the numeric side-conditions.}

@item{This line separates the premises from the conslusion.}

@item{The conclusion specifies the macro's outputs: an untyped Video term
@racket[(untyped-video:λ (x- ...) e-)] along with its type @racket[(∀ (n
...) (→ τ ... τ_out #:when (and C new-Cs)))]. In Turnstile, types are
represented using the same syntax structures as terms.}

]

The second part of @figure-ref{type-checking-macros} presents Typed Video's
type-checking function application macro. It naturally interposes on Racket's
function application hook, @racket[#%app] (via untyped Video's function
application definition), to add type checking. Here is a brief description of
each line of @racket[#%app]:

@itemlist[#:style 'ordered

@item{The input syntax is matched against pattern @racket[(#%app e_fn e_arg
...)], binding the function to @racket[e_fn] and all arguments to @racket[e_arg].}

@item{The function elaborates to @racket[e_fn-] while its type must match
@racket[(∀ Xs (→ τ_inX ... τ_outX #:when CX))], which is universally quantified
over type variables @racket[Xs] and has side-condition @racket[CX].}

@item{The macro peforms local type inference, computing the concrete types
@racket[τs] at which to instantiate the polymorphic function. This call to @racket[solve] may use any constraint solver.}

@item{Next, the polymorphic function type is instantiated to concrete types
@racket[(τ_in ... τ_out)] and a concrete side-condition @racket[C].}

@;item{The side-condition @racket[C] is reduced to a canonical form @racket[C*].}

@item{If @racket[C] is @racket[false], stop and report an error. Though this
paper truncates the code, Typed Video presents more details when reporting
errors to the user. This line demonstrates how our DSL creates true
abstractions, reporting errors in terms of the surface language rather than the
host language.}

@item{If @racket[C] is still an expression, propagate it.}

@item{Check that the function arguments have the instantiated types. This
premise uses the ``check'' left bidirectional arrow. If a programmer does not
explicitly implement a left-arrow version of a rule, Turnstile uses a standard
subsumption rule by default.}

@item{The conclusion line indicates the end of the premises.}

@item{The output of the macro consists of an untyped Video term along
with its computed type.}

]

The rest of the type system implementation resembles the rules in
@figure-ref{type-checking-macros} and mostly adds type checking to the Video
primitives described earlier in the paper.
