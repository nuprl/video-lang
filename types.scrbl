#lang scribble/sigplan

@title[#:tag "types"]{Inspector Gadget}
@(require (except-in scribble/manual cite) scribble/core racket/list "bib.rkt")

What use is a programming language without a dependent type system? Lots of
course, as Video shows. After all, Video xis a scripting language, and most
description of a conference video are no longer than a few lines. No real
programmer needs types for such programs. For our typed friends, however,
the existence of an untyped language might be inconceivable, and we
therefore whipped together a dependent type system and its implementation
in a single work day. 

After explaining an imaginary rationale for a type system (secref{video-data}),
the section presents the essential idea, type checking the length of producers,
transitions (which are conceptually functions on producers), functions on
transitions, and so on (@secref{index}). Next it is time to show some
type-checking rules (@secref{type-system}). The final step is to once again
demonstrate the power of Racket's syntax system, which cannot only modify the
syntax of a base language but also add a type system---with more or less the
familiar notation found in papers on fancy type systems (@secref{type-implementation}). 

@; -----------------------------------------------------------------------------
@section[#:tag "video-data"]{Video Data Types}

Video programs primarily manipulate two types of data, producers and
transitions. A typical Video program slices and combines these values together
so unsurprisingly, programmer errors often involve manipulating producers and
transitions of improper lengths. For example, the following piece of code
mistakenly tries to extract 16 frames from a producer that is only 10 frames
long:
@;
@racketblock[(cut-producer (color "green" #:length 10) #:start 5 #:end 20)
             @code:comment{CRASH (given producer must have length >= 16)}]
@;
This second example attempts to use producers too short to incorporate the specified
transition:
@;
@racketblock[(playlist (blank #:length 10) (fade-transition #:length 15) (color #:length 10))
             @code:comment{CRASH (given producers must have length >= 15)}]
@;
While we could rely on dynamic checks to enforce length invariants, they might
still generate errors too late, since rendering a video is the final step in
the video editing process. Thus, we turn to a static type system.

@section[#:tag "index"]{Length Indexes}

To address this problem, we introduce Typed Video, which adds a lightweight
dependent type system to Video. In Typed Video, the types of producers and
transitions are indexed by a non-negative integer term corresponding to their lengths. The
type system resembles a simplified version of Xi and Pfenning's ATS@cite[ats-pldi].

Such a type system does not impose too much of a burden in our domain of video
editing and works well in practice since Video programmers are already
accustomed to specifying explicit length information in their programs. With
Typed Video, the examples from @secref{video-data} produce static type error
messages. In general, our type system ensures that producer values do not flow
into positions where their length is less than expected.

Typed Video also supports writing functions polymorphic in video
lengths. Continuing the conference video editing example from earlier in the
paper (@secref{overview}), a function @racket[add-slides] for combining the
video of a speaker with their slides might have the following signature.

@racketblock[(define (add-slides {n} [video : (Producer n)] [slides : (Producer n)] -> (Producer n))
               (multitrack video slides background #:length (producer-length video)))]
@;
This function binds a universally-quantified type variable @racket[n] (in Typed
Video, type variables may only be bound to integer terms) and uses it to
specify that the lengths of the input and output producers must match up.

In addition, a programmer may specify side-conditions involving these type
variables. Here is an @racket[add-intro] function that adds an
opening and ending sequence to a speaker's video.

@racketblock[(code:comment "Add conference logos to the front and end of a video.")
             (define (add-intro {n} [main-talk : (Producer n)] #:when (>= n 400) -> (Producer (+ n 600)))
               (playlist begin-clip
                         @fade-transition[#:length 200]
                         main-talk
                         @fade-transition[#:length 200]
                         end-clip)
               (define begin-clip @image["logo.png" #:length 500])
               (define end-clip @image["logo.png" #:length 500]))]
@;
The @racket[add-intro] function specifies, with a @racket[#:when] keyword, that
its input must be a producer of at least 400 frames, due to the use of two
200-frame transitions, and that the output adds 600 frames to the input,due to
the added intro and outro, minus the transition frames.

Programmer-specified side-conditions may propagate to other
functions. For example, here is a @racket[make-conference-talk]
function (first introduced in @secref{overview}):

@racketblock[(define (make-conference-talk {n} [video :  (Producer n)]
                                               [slides : (Producer n)]
                                               [audio :  (Producer n)]
                                               [offset : Int] -> (Producer (+ n 600)))
               @code:comment{...}
               (define p1 (add-slides video slides))
               (define p2 (add-intro p1))
               @code:comment{...})]
@;
Though @racket[make-conference-talk] does not specify an explicit
side-condition, it inherits the @racket[(>= n 400)] side-condition due to the
call to @racket[add-intro]. Thus applying @racket[make-conference-talk] to a
video shorter than 400 frames results in a type error.

@section[#:tag "type-system"]{The Type System}

@(define (mk-txt x) (list "\\texttt{" x "}"))
@(define (inferrule . as)
   (define args (map mk-txt as))
   (make-element (make-style "inferrule" '(exact-chars))
                 (cond [(= 1 (length args))
                        (list " }{" (mk-txt (car args)))]
                       [(>= (length args) 2) @; scribble auto flattens?
                        (define sgra (reverse args))
                        (define conclusion (car sgra))
                        (define premises (reverse (add-between (cdr sgra) "\\\\")))
                        (list premises "}{" conclusion)]
                       [else (printf "shouldnt get here, args = ~a\n" args)])))

This subsection presents a taste of Typed Video's type system. We omit a
full formal presentation since (1) it is not the main object of study in
this paper, and (2) Typed Video utilizes only existing type system methods.

As previously mentioned, Video progammers already specify explicit video
lengths in their programs and thus it is easy to lift this information to the
type-level. For example, here are, roughly, a few rules for creating and
consuming producers:

@inferrule["e : String"]{(color e \#:length n) : (Producer n)}

@inferrule["e : String"]{(color e) : Producer}

@inferrule["f : File" "|f| = n"]{(clip f) : (Producer n)}

@inferrule["p/t <: (Producer n) \\textrm{or} p/t <: (Transition m)" "..."]{(playlist p/t ...) : (Producer (- (+ n ...) (+ m ...)))}

The first rule lifts the specified length to the expression's type. In the
absence of a length argument, as in the second rule, the expression has type
@tt{Producer}, which is sugar for @tt{(Producer ∞)}. The third says that if the
given file @tt{f} on disk points to a video of length @tt{n}, then an
expression @tt{(clip f)} has type @tt{(Producer n)}.

The fourth rule shows how producer lengths may be combined. Specifically, a
@racket[playlist] appends producers together and thus their lengths are
summed. A playlists arguments may also have transitions interleaved between the
producers. Since each transition results in an overlapping of producers, the
lengths of the transitions are subtracted from the total. A type error results
if the computed length of a producer is negative.

The fourth rule uses Typed Video's subtyping relation. Here is the subtyping
rule for the @tt{Producer} type:

@inferrule["m >= n"]{(Producer m) <: (Producer n)}

Since Typed Video aims to prevent not-enough-video errors, it is acceptable to supply
a producer that is longer than expected but not shorter.

In addition to requiring non-negative video lengths, Typed Video imposes
additional restrictions on the terms that may appear in a @tt{Producer}
type. For example, application of arbitrary functions is disallowed to prevent
non-termination while type checking. In general, Typed Video normalizes types
to a canonical form before type checking, a process that includes partially
evaluating arithmetic expressions as well as flattening associative and sorting
commutative operations like addition. If the @tt{Producer} type constructor is
applied to an unsupported term, the type defaults to a @tt{Producer} of
infinite length.

@section[#:tag "type-implementation"]{Type Systems as Macros}

Thanks to reuse of linguistic components, implementing Typed Video required one
work-day's worth of effort. Specifically, we reused the infrastructure of
Racket's macro system in order to implement type checking, following the "Type
Systems as Macros" approach@cite[tsam-popl]. As a result, Typed Video is an
extension to, rather than a reimplementation, of the Video language. Further,
Creating Typed Video required no changes to Video.

@Secref{implementation} demonstrates language creation as a series of syntactic
extensions. Our type system directly extends these macros with type
checking. The rest of the section briefly presents the
@racket[λ] and function application type rule implementations. Here is
@racket[λ]:

@codeblock[#:line-numbers 1]{
(define-syntax/typecheck (λ {n ...} ([x : τ] ... #:when C) e) ≫
  [(n ...) ([x ≫ x- : τ] ...) ⊢ e ≫ e- ⇒ τ_out]
  #:with new-Cs (get-captured-Cs e-)
  ---------
  [⊢ (Video:λ (x- ...) e-) ⇒ (∀ (n ...) (→ τ ... τ_out #:when (and C new-Cs)))])}

We implement Typed Video with Turnstile, a Racket DSL
(introduced in@cite[tsam-popl]) for creating Typed DSLs using a concise,
bidirectional type-judgement-like syntax, as seen in the @racket[λ]
definition. Turnstile introduces the @racket[define-syntax/typecheck] form,
which defines a macro that incorporates type checking as part of macro
processing. Interleaving type checking and macros in this manner not only
simplifies implementation of the type system, but also enables creating true
abstractions on top of the host language. Contrast this with typed-DSL creation
other functional languages, where type errors are often reported in host
language terms.

Next we briefly explain each line of the definition:

@itemlist[#:style 'ordered

@item{The type checking macro's input must match pattern @racket[(λ {n} ([x :
τ] ... #:when C) e)], which binds pattern variables @racket[n] (the type
variable), @racket[x] (the λ parameters), @racket[τ] (the type annotations),
@racket[C] (a side-condition), and @racket[e] (the lambda body). Pattern
variables may be subsequently used to construct new program fragments. An
ellipses pattern matches zero-or-more of its preceding pattern any pattern
variables in that pattern requires ellipses when subsequently used.}

@item{Since type checking is interleaved with macro processing, Turnstile
judgements must reflect this as well. Thus, a Turnstile judgement @racket[[ctx
⊢ e ≫ e- ⇒ τ]] is read "in context @racket[ctx], @racket[e] elaborates to
@racket[e-] and has type @racket[τ]". Specifically, the lambda body @racket[e]
elaborates to to @racket[e-], simultaneously computing its type @racket[τ_out].

This elaboration and type checking occurs in the context of the necessary
variables. Turnstile reuses Racket's lexical scoping to implement the type
environment and thus a programmer writes only new environment bindings (i.e.,
explicit "Γ"s are not needed). Specifically, the premise uses two type
envionments, one each for the type variables and lamba parameters,
respectively, where the latter may contain references to the former.}

@item{A @racket[#:with] premise binds additional pattern variables. Here,
elaborating the lambda body @racket[e] may generate additional side-conditions,
@racket[new-Cs], that must be satisfied by the function's inputs. Turnstile
allows specifying propagation of not just types, but arbitrary metadata on the
program tree, and we use this mechanism to compute the extra side-conditions.}

@item{} @item{Finally, the outputs of our type checking macro is below the
conclusion line: an untyped Video term @racket[(video:λ (x- ...) e-)] and along
with its type @racket[(∀ (n ...) (→ τ ... τ_out #:when (and C new-Cs)))]. In
Turnstile, types are represented using the same syntax data structures as
terms.}

]

Here is our type-checking function application macro definition. To support
linguistic reuse, Racket's compiler exposes overrideable hooks for most of its
core forms. Here, we redefine the function application hook @racket[#%app], to
add type checking.

@codeblock[#:line-numbers 1]{
(define-syntax/typecheck (#%app e_fn e_arg ...) ≫ 
   [⊢ e_fn ≫ e_fn- ⇒ (∀ Xs (→ τ_inX ... τ_outX #:when CX))]
   #:with τs (solve Xs (τ_inX ...) (e_arg ...))
   #:with (τ_in ... τ_out C) (inst τs Xs (τ_inX ... τ_outX CX))
   #:with C* (type-eval C)
   #:fail-unless C* "failed side-condition"
   #:unless (boolean? C*) (add-C C*)
   [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
   --------
   [⊢ (Video:#%app e_fn- e_arg- ...) ⇒ τ_out])}

Roughly, here is a brief description of each line of the definition.
@itemlist[#:style 'ordered

@item{The input syntax is matched against pattern @racket[(#%app e_fn e_arg
...)], binding the function to @racket[e_fn] and the arguments to @racket[e_arg
...].}

@item{The function elaborates to @racket[e_fn-] and its type matches pattern
@racket[(∀ Xs (→ τ_inX ... τ_outX #:when CX))], which is universally quantified
over type variables @racket[Xs] and has side-condition @racket[CX].}

@item{The macro peforms local type inference, computing the concrete types
@racket[τs] at which to instantiate the polymorphic function.}

@item{Next, the polymorphic function type is instantiated to concrete types
@racket[(τ_in ... τ_out)] and a concrete side-condition @racket[C].}

@item{The side-condition @racket[C] is reduced to a canonical form @racket[C*].}

@item{If @racket[C*] is @racket[false], stop and report an error. This line
demonstrates how our DSL creates true abstractions, since the error is reported
in terms of the surface language rather than the host language.}

@item{If @racket[C*] is still an exprsesion, propagate it.}

@item{Check that the function arguments have the instantiated types. This
premise uses the
"check" left bidirectional arrow. If a programmer does not explicitly implement
a left-arrow version of a rule, Turnstile uses a standard subsumption rule by
default.}

@item{} @item{The outputs of the macro are an untyped Video term along
with its computed type.}

]
