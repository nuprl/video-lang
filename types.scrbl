#lang scribble/acmart

@title[#:tag "types"]{The Bodyguard}
@(require (except-in scribble/manual cite)
          "utils.rkt"
          scriblib/figure scriblib/footnote
          (except-in scribble/core paragraph)
          racket/list "bib.rkt")

What use is a programming language without a dependent type system? Lots of
course, as Video shows. After all, Video is a scripting language, and most
descriptions of a conference video are no longer than a few lines. No real
programmer needs types for such programs. For some, however,
the existence of an untyped language might be inconceivable, and we
therefore whipped together a dependent type system in a single work day. 

After explaining the rationale for such a type system (@secref{video-data}), we
present the essential idea: type checking the length of producers,
transitions (which are conceptually functions on producers), functions on
transitions, and so on (@secref{index}). Then we examine a few type-checking
rules (@secref{type-system}). Finally, we once again demonstrate the power of
Racket's syntax system, which not only modifies the syntax of a base language
but also adds a type system---with more or less the familiar notation found in
papers on fancy type systems
(@secref{type-implementation}). 

@; -----------------------------------------------------------------------------
@section[#:tag "video-data"]{Video Data Types}

Video programs primarily manipulate two types of data: producers and
transitions. A typical Video program slices these values and then combines
them.  Not surprisingly, programmer errors often involve manipulating producers
and transitions of improper lengths. Such errors are particularly dangerous
because they often manifest themselves at the C level during rendering. For
example, the following piece of code mistakenly tries to extract 16 frames from
a producer that is only 10 frames long:
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
While old fashioned scripting languages generally rely on dynamic checks to
enforce such conditions, modern scripting languages use a static type system
instead@cite[meijer-jfp #;meijer-icfp haskell-scripting-cufp]. So does Typed
Video.

@section[#:tag "index"]{Length Indexes}

Typed Video adds a lightweight dependent type system to Video, where the types
of producers and transitions are indexed by natural-number terms
corresponding to their lengths. The rest of the type system resembles a
simplified version of Xi and Pfenning's ATS@cite[ats-pldi].

Such a type system does not impose too much of a burden in our domain.
Indeed, it works well in practice because Video programmers are
already accustomed to specifying explicit length information in their
programs. For example, the snippets from @secref{video-data} produce static
type error messages in Typed Video. In general, the type system ensures that
producer values do not flow into positions where their length is less than
expected.

Typed Video also supports writing functions polymorphic in the lengths of
videos. Continuing the conference video editing example from @secref{overview},
a function @racket[add-slides] for combining the video of a speaker with
slides from their presentation might have the following signature:

@racketblock[
(define (add-slides {n} [video : (Producer n)] [slides : (Producer n)] -> (Producer n))
  (multitrack video slides background #:length (producer-length video)))]
@;
This function binds a universally-quantified type variable @racket[n] that
ranges over natural numbers and uses it to specify that the lengths of the
input and output producers must match up.

In addition, a programmer may specify side-conditions involving type
variables. Here is the @racket[add-bookend] function, which adds an
opening and ending sequence to a speaker's video:

@(nested (minipage
@racketblock[
(code:comment "Add conference logos to the front and end of a video.")
(define (add-bookend {n} [main-talk : (Producer n)] #:when (>= n 400) -> (Producer (+ n 600)))
  (playlist begin-clip @fade-transition[#:length 200]
	    main-talk
	    @fade-transition[#:length 200] end-clip)
  (define begin-clip @image["logo.png" #:length 500])
  (define end-clip @image["logo.png" #:length 500]))]))
@;
The @racket[add-bookend] function specifies with a @racket[#:when] keyword that
its input must be a producer of at least 400 frames because it uses two
200-frame transitions. The result type says that the output adds 600
frames to the input. Here the additional frames come from the added beginning
and end segments, minus the transition frames.

Programmer-specified side-conditions may propagate to other functions. The
@racket[conference-talk] function from @secref{overview} benefits from
this propagation:

@racketblock[
(define (conference-talk {n} [video  : (Producer n)]
	                     [slides : (Producer n)]
			     [audio  : (Producer n)]
			     [offset : Int]
           -> (Producer (+ n 600)))
  @code:comment{...}
  (define p1 (add-slides video slides))
  (define p2 (add-bookend p1))
  @code:comment{...})]
@;
Even though @racket[conference-talk] does not specify an explicit
side-condition, it inherits the @racket[(>= n 400)] side-condition from
@racket[add-bookend]. Thus applying @racket[conference-talk] to a video
that is not provably longer than 400 frames results in a type error:
@;
@racketblock[
(conference-talk (blank 200) (blank 200) (blank 200) 0)
@code:comment{TYPE ERROR: Failed condition (>= n 400), inferred n = 200}]

@section[#:tag "type-system"]{The Type System}

@(define (mk-txt x) (list "\\texttt{" x "}"))
@(define (ensuremath x)
   (make-element (make-style "ensuremath" '(exact-chars)) x))
@(define (inferrule #:name [name ""] . as)
   (define args (map mk-txt as))
   (ensuremath
   (make-element (make-style (string-append "inferrule[" name "]")
                             '(exact-chars))
                 (cond [(= 1 (length args))
                        (list " }{" (mk-txt (car args)))]
                       [(>= (length args) 2) @; scribble auto flattens?
                        (define sgra (reverse args))
                        (define conclusion (car sgra))
                        (define premises (reverse (add-between (cdr sgra) "\\\\")))
                        (list premises "}{" conclusion)]
                       [else (printf "shouldnt get here, args = ~a\n" args)]))))

@(define (noindent)
   (make-element (make-style "noindent" '(exact-chars)) null))
@(define (hspace x)
   (make-element (make-style "hspace" '(exact-chars))
                 (string-append x "pt")))
@(define (vspace x)
   (make-element (make-style "vspace" '(exact-chars))
                 (string-append x "pt")))

While Typed Video utilizes only existing type system ideas, it is nevertheless
instructive to inspect a few of its rules before we explain how to turn them
into a language implementation.

@(define (sc str) (elem str #:style (style "textsc" '(exact-chars))))

As previously mentioned, Video programmers already specify explicit video
lengths in their programs and thus it is easy to lift this information to the
type-level. For example, @figure-ref{type-rules} presents, roughly, a few rules
for creating and consuming producers.  The @sc{Color-n} rule lifts the
specified length to the expression's type. In the absence of a length argument,
as in the @sc{Color} rule, the expression has type @code{Producer}, which is
syntactic sugar for @code{(Producer ∞)}. The @sc{Clip} rule says that if the
given file @tt{f} on disk points to a video of length @code{n},@note{Obviously,
the soundness of our type system is now contingent on the correctness of this
system call.} then an expression @code{(clip f)} has type @tt{(Producer
n)}. The @sc{Playlist} rule shows how producer lengths may be
combined. Specifically, a @racket[playlist] appends producers together and thus
their lengths are summed. If playlists interleave transitions between
producers, the lengths of the transitions are subtracted from the total because
each transition results in an overlapping of producers. A type error is
signaled if the computed length of a producer is negative.

@figure["type-rules" @list{A few type rules for Typed Video}]{
@centered[
@inferrule[#:name "color-n" "$\\Gamma\\vdash$ e : String"]{$\Gamma\vdash$ (color e \#:length n) : (Producer n)}
@hspace{64}
@inferrule[#:name "clip" "$\\Gamma\\vdash$ f : File" "|f| = n"]{$\Gamma\vdash$ (clip f) : (Producer n)}
]
@vspace{10}
@centered[
@inferrule[#:name "color" "$\\Gamma\\vdash$ e : String"]{$\Gamma\vdash$ (color e) : Producer}
@hspace{16}
@inferrule[#:name "playlist" "$\\Gamma\\vdash$ p/t : $\\tau$"
                             "$\\tau$ <: (Producer n) \\textrm{or} $\\tau$ <: (Transition m)" "..."]{
                              $\Gamma\vdash$ (playlist p/t ...) : (Producer (- (+ n ...) (+ m ...)))}]
}

The Playlist rule uses Typed Video's subtyping relation. Here is the subtyping
rule for the @code{Producer} type:

@centered[@inferrule["m >= n"]{(Producer m) <: (Producer n)}]

@(noindent)Since Typed Video aims to prevent not-enough-frame errors, it is
acceptable to supply a producer that is longer than expected but not shorter.

In addition to requiring non-negative video lengths, Typed Video imposes
additional restrictions on the terms that may appear in a @code{Producer}
type. For example, application of arbitrary functions is disallowed to prevent
non-termination; only addition, subtraction, and a few Video primitives are
supported, which simplifies type checking. If the @code{Producer} type
constructor is applied to an unsupported natural-number term, the type defaults
to a @code{Producer} of infinite length. Similar restrictions are imposed on
side-conditions. Despite these restrictions, Typed Video works well in practice
and can type check all our example programs, including those for the RacketCon
2016 video proceedings.

@; -----------------------------------------------------------------------------
@section[#:tag "type-implementation"]{Type Systems as Macros}

The implementation of Typed Video relies on linguistic reuse to produce a
full-fledged programming language without starting from scratch. Specifically,
it reuses Racket's syntax system to implement type checking, following
@nonbreaking{@cite-author[tsam-popl]'s (@cite-year[tsam-popl])} type-systems-as-macros technique. As a result, Typed Video
is an extension to, rather than a reimplementation of, the untyped Video
language.

@Figure-ref{type-checking-macros} shows the implementation of two interesting
rules: @racket[λ] and function application. The @racket[require] at the top of
the figure imports and prefixes the identifiers from untyped Video, i.e., the
syntactic extensions from @secref{implementation}, which are used, unmodified,
to construct the output of the type-checking pass.

@(define *line-no 0)
@(define (line-no)
   (set! *line-no  (+ *line-no 1))
   (define line-no (format (if (< *line-no 10) "0~a " "~a ") *line-no))
   @exact{\tt @line-no})

@figure["type-checking-macros" @list{Type-checking via syntax transformers}]{
@racketmod[
turnstile

@#,line-no[](require (prefix-in untyped-video: video)) ; imports untyped Video identifiers with a prefix
@#,line-no[](provide λ #%app) ; exports Typed Video identifiers
@#,line-no[]
@#,line-no[](define-syntax/typecheck (λ {n ...} ([x : τ] ... #:when C) e) ≫
@#,line-no[]  [(n ...) ([x ≫ x- : τ] ...) ⊢ e ≫ e- ⇒ τ_out]
@#,line-no[]  #:with new-Cs (get-captured-Cs e-)
@#,line-no[]  ----------------------------------
@#,line-no[]  [⊢ (untyped-video:λ (x- ...) e-) ⇒ (∀ (n ...) (→ τ ... τ_out #:when (and C new-Cs)))])
@#,line-no[]
@#,line-no[](define-syntax/typecheck (#%app e_fn e_arg ...) ≫ 
@#,line-no[]   [⊢ e_fn ≫ e_fn- ⇒ (∀ Xs (→ τ_inX ... τ_outX #:when CX))]
@#,line-no[]   #:with solved-τs (solve Xs (τ_inX ...) (e_arg ...))
@#,line-no[]   #:with (τ_in ... τ_out C) (inst solved-τs Xs (τ_inX ... τ_outX CX))
@#,line-no[]   #:fail-unless (not (false? C)) "failed side-condition"
@#,line-no[]   #:unless (boolean? C) (add-C C)
@#,line-no[]   [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
@#,line-no[]   ------------------------------
@#,line-no[]   [⊢ (untyped-video:#%app e_fn- e_arg- ...) ⇒ τ_out])
]
}

We implement our type checker with Turnstile, a Racket DSL introduced by Chang
et al. for creating Typed DSLs. This DSL-generating-DSL uses a concise,
bidirectional type-judgement-like syntax, as seen in
@figure-ref{type-checking-macros}'s @racket[define-syntax/typecheck]
rules. These rules define syntax transformers that incorporate type checking as
part of syntax elaboration. Interleaving type checking and elaboration in
this manner not only simplifies implementation of the type system, but also
enables creating true abstractions on top of the host language.

Next we briefly explain each line of the @racket[λ] definition:

@(define current-line (make-parameter 1))
@(define (inc-line) (current-line (add1 (current-line))))
@(define (linelabel x . rst)
   (apply para
          @exact{\vspace{0.2cm}}
     (noindent)
     (bold "line ") (bold (number->string x)) ": " rst))
@(define (with-linelabel . rst)
   (begin0 (apply linelabel (current-line) rst) (inc-line)))

@(current-line 4)

@with-linelabel{The type-checking transformer's input must match pattern
@racket[(λ {n ...} ([x : τ] ... #:when C) e)], which binds pattern variables
@racket[n] (the type variables), @racket[x] (the λ parameters), @racket[τ] (the
type annotations), @racket[C] (a side-condition), and @racket[e] (the lambda
body).}

@; if the readers don't understand this by now, we're lost
@;{Pattern variables may be used to construct new program
fragments, e.g., the outputs of the transformer. An ellipses pattern matches
zero-or-more of its preceding pattern; any pattern variables in that pattern
requires ellipses when used.}

@with-linelabel{Since type checking is interleaved with syntax elaboration,
Turnstile type judgements are elaboration judgements as well. Specifically, a
Turnstile judgement @racket[[ctx ⊢ e ≫ e- ⇒ τ]] is read ``in context
@racket[ctx], @racket[e] elaborates to @racket[e-] and has type @racket[τ].''

Thus the lambda body @racket[e] elaborates to to @racket[e-], simultaneously
computing its type @racket[τ_out]. This elaboration and type checking occurs in
the context of the free variables. Instead of propagating a type environment,
Turnstile reuses Racket's lexical scoping to implement the type
environment. This re-use greatly enhances the compositionality of languages
and reduces effort so that a programmer gets away with specifying only new
environment bindings. Specifically, the first premise uses two type environments, one
each for the type variables and lambda parameters, respectively, where the
latter may contain references to the former.}

@with-linelabel{A @racket[#:with] premise binds additional pattern
variables. Here, elaborating the lambda body @racket[e] may generate additional
side-conditions, @racket[new-Cs], that must be satisfied by the function's
inputs. Turnstile allows specifying propagation of not just types, but
arbitrary metadata on the program tree, and we use this mechanism to compute
the numeric side-conditions.}

@with-linelabel{These dashes separate the premises from the conslusion.}

@with-linelabel{The conclusion specifies the transformer's outputs: an untyped
Video term @racket[(untyped-video:λ (x- ...) e-)] along with its type
@racket[(∀ (n ...) (→ τ ... τ_out #:when (and C new-Cs)))]. In Turnstile, types
are represented using the same syntax structures as terms.}

@exact{\vspace{0.4cm}}

The second part of @figure-ref{type-checking-macros} presents Typed Video's
function application rule. It naturally interposes on Racket's
function application hook, @racket[#%app], via untyped Video's function
application definition, to add type checking. Here is a brief description of
each line of @racket[#%app]:

@(inc-line)

@with-linelabel{The input syntax is matched against pattern @racket[(#%app e_fn
e_arg ...)], binding the function to @racket[e_fn] and all arguments to
@racket[e_arg].}

@with-linelabel{The function elaborates to @racket[e_fn-] with type @racket[(∀
Xs (→ τ_inX ... τ_outX #:when CX))], which is universally quantified over type
variables @racket[Xs] and has side-condition @racket[CX].}

@with-linelabel{The transformer peforms local type inference, computing the
concrete types @racket[solved-τs] at which to instantiate the polymorphic
function. This call to @racket[solve] may use any constraint solver.}

@with-linelabel{Next, the polymorphic function type is instantiated to concrete
types @racket[(τ_in ... τ_out)] and a concrete side-condition @racket[C].}

@;item{The side-condition @racket[C] is reduced to a canonical form @racket[C*].}

@with-linelabel{If @racket[C] is @racket[false], stop and report an
error. Though this paper truncates the code, Typed Video presents more details
when reporting errors to the user. This line demonstrates how our DSL creates
true abstractions, reporting errors in terms of the surface language rather
than the host language.}

@with-linelabel{If @racket[C] is still an expression, propagate it.}

@with-linelabel{Check that the function arguments have the instantiated
types. This premise uses the ``check'' left bidirectional arrow. If a
programmer does not explicitly implement a left-arrow version of a rule,
Turnstile uses a standard subsumption rule by default.}

@(inc-line)

@with-linelabel{The generated code consists of an untyped Video term along
with its computed type.} 

@exact{\vspace{0.4cm}}

The rest of the implementation is similar. For example, implementing
polymorphism is straightforward because Turnstile reuses Racket's knowledge
of a program's binding structure to automatically handle naming. Further,
the ``as macros'' approach facilitates implementation of rules for both
terms and types, and thus the implementation type-level computations also
resembles the rules in @figure-ref{type-checking-macros}.

