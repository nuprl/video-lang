#lang scribble/acmart

@title[#:tag "types"]{The Bodyguard}
@(require (except-in scribble/manual cite)
          (except-in pict table)
          "utils.rkt"
          scriblib/figure scriblib/footnote
          (except-in scribble/core paragraph)
          racket/list "bib.rkt"
          #;(for-label (except-in video #%module-begin)
                     (except-in turnstile #%module-begin mk-~ mk-?)))

What use is a programming language without a dependent type system? Lots of
course, as Video shows. After all, Video is a scripting language, and most
descriptions of a conference video are no longer than a few lines. No real
programmer needs types for such programs. For some readers, however, the
existence of an untyped language might be inconceivable, and we therefore
whipped together a dependent type system in a single work day.@note{We do not
explore the metatheory of our type system since it is beyond the scope of this
pearl.}

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
transitions. A typical program slices these values and then combines
them. Not surprisingly, errors often involve manipulating producers
and transitions of improper lengths. Such errors are particularly dangerous
because they often manifest themselves at the FFI level during rendering. For
example, the following piece of untyped code mistakenly tries to extract 15 frames from
a producer that is only 10 frames long:
@;
@(split-minipage
  #:split-location 0.99
@racketblock[
(cut-producer (color "green" #:length 10) #:start 0 #:end 15)
@code:comment{EXCEPTION: given producer must have length >= end - start = 15}]
(blank 1 1))
@;
The following second example attempts to use producers that are too short for
the specified transition:
@;
@(split-minipage
  #:split-location 0.99
@racketblock[
(playlist (blank 10) (fade-transition #:length 15) (color "green" #:length 10))
@code:comment{EXCEPTION: given producers must have length >= transition length 15}]
(blank 1 1))
@;
While old-fashioned scripting languages rely on dynamic checks to
catch these bugs, modern scripting languages use a static type system
instead@cite[meijer-jfp #;meijer-icfp haskell-scripting-cufp]. So does Typed
Video.

@section[#:tag "index"]{Length Indexes}

Typed Video adds a lightweight dependent type system to Video, where the types
of producers and transitions are indexed by natural-number terms
corresponding to their lengths. The rest of the type system resembles a
simplified version of Dependent ML@cite[ats-pldi].

Such a type system does not impose too much of a burden in our domain.
Indeed, it works well in practice because Video programmers are
already accustomed to specifying explicit length information in their
programs. For example, the snippets from the preceding section produce static
type error messages in Typed Video:
@;
@(split-minipage
  #:split-location 0.99
@racketblock[
(cut-producer (color "green" #:length 10) #:start 0 #:end 15)
@code:comment{TYPE ERR: cut-producer: expected (Producer 15), given (Producer 10)}
(playlist (blank 10) (fade-transition #:length 15) (color "green" #:length 10))
@code:comment{TYPE ERR: playlist: (fade-transition #:length 15) too long for producer (blank 10)}]
(blank 1 1))
@;
In general, the type system ensures that
producer values do not flow into positions where their length is less than
expected.

Typed Video also supports writing functions polymorphic in the lengths of
videos. Recall the @racket[conference-talk] example from
@figure-ref{video-functions}. A function @racket[add-slides] that just combines
the video of a speaker with a video of slides from their presentation might
look like this:

@require{pictures.rkt}

@vspace{2}

@split-minipage[
 #:split-location 0.99
@racketblock[
(define (add-slides {n} [vid : (Producer n)] [slides : (Producer n)] -> (Producer n))
  (multitrack vid #,elided slides #,elided))]
 (blank 1 1)
 ]

@vspace{1}

@(noindent)The function's type binds a universally-quantified type index
variable @racket[n] that ranges over natural numbers and uses it to specify
that the lengths of the input and output producers must match up.

In addition, a programmer may specify side-conditions involving type index
variables. Here is an @racket[add-bookend] function, which adds an
opening and ending sequence to a speaker's video:

@;vspace{4}
@(split-minipage
  #:split-location 0.99
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
(blank 1 1))

@vspace{2}

@(noindent)The @racket[add-bookend] function specifies, with a @racket[#:when]
keyword, that its input must be a producer of at least 400 frames because it
uses two 200-frame transitions. The result type says that the output adds 600
frames to the input. The additional frames come from the added beginning
and end segments, minus the transition frames.

Programmer-specified side-conditions may propagate to other functions. The
@racket[conference-talk] function from @secref{overview} benefits from
this propagation:

@vspace{2}

@(split-minipage
  #:split-location 0.99
@racketblock[
(define (conference-talk {n} [video  : (Producer n)] [slides : (Producer n)]
                             [audio  : (Producer n)] [offset : Int]
                             -> (Producer (+ n 600)))
  #,elided
  (define p1 (add-slides video slides))
  (define p2 (add-bookend p1))
  #,elided)]
(blank 1 1))

@vspace{2}

@(noindent)Even though @racket[conference-talk] does not specify a
side-condition, it inherits the @racket[(>= n 400)] side-condition from
@racket[add-bookend]. Thus applying @racket[conference-talk] to a video
that is not provably longer than 400 frames results in a type error:
@;
@(split-minipage
  #:split-location 0.99
@racketblock[
(conference-talk (blank 200) (blank 200) (blank 200) 0)
@code:comment{TYPE ERROR: Failed condition (>= n 400), inferred n = 200}]
(blank 1 1))

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
lengths in their programs, and it thus is easy to lift this information to the
type level. For example, @figure-ref{type-rules} shows a few rules
for creating and consuming producers. The @sc{Color-n} rule lifts the
specified length to the expression's type. In the absence of a length argument,
as in the @sc{Color} rule, the expression has type @code{Producer}, which is
syntactic sugar for @code{(Producer ∞)}. The @sc{Clip} rule says that if the
given file @tt{f} on disk points to a video of length @code{n},@note{Obviously,
the soundness of our type system is now contingent on the correctness of this
system call.} then an expression @code{(clip f)} has type @code{(Producer n)}.

@figure["type-rules" @list{A few Producer type rules for Typed Video}]{
@centered[
@inferrule[#:name "Color-n" "$\\Gamma\\vdash$ e : String"]{$\Gamma\vdash$ (color e \#:length n) : (Producer n)}
@hspace{24}
@inferrule[#:name "Color" "$\\Gamma\\vdash$ e : String"]{$\Gamma\vdash$ (color e) : Producer}
]
@vspace{10}
@centered[
@inferrule[#:name "Clip" "$\\Gamma\\vdash$ f : File" "|f| = n"]{$\Gamma\vdash$ (clip f) : (Producer n)}
]
@vspace{10}
@centered[
@inferrule[#:name "Playlist" "$\\forall$p/t: $\\Gamma\\vdash$ p/t : (Producer n) $\\textrm{or}$ $\\Gamma\\vdash$ p/t : (Transition m)"
                             "where each transition must occur between two producers"]{
                              $\Gamma\vdash$ (playlist p/t ...) : (Producer (- (+ n ...) (+ m ...)))}]
}

Typed Video utilizes a standard subtyping relation with a few additions,
e.g., for @code{Producer}s:

@centered[@inferrule["m >= n"]{(Producer m) <: (Producer n)}]

@(noindent)Since Typed Video aims to prevent not-enough-frame errors, it is
acceptable to supply a producer that is longer, but not shorter, than expected.

The @sc{Playlist} rule in @figure-ref{type-rules} shows how producer lengths
may be combined. Specifically, a @racket[playlist] appends producers together
and thus their lengths are summed. If playlists interleave transitions between
producers, the lengths of the transitions are subtracted from the total because
each transition results in an overlapping of producers. A type error is
signaled if the computed length of a producer is negative, as specified in the
kinding rule for @code{Producers}:

@centered[@inferrule["$\\Gamma\\vdash$ n : Int $\\textrm{and}$ n $\\textrm{is a well-formed index expression}$" "n >= 0"]{$\Gamma\vdash$ (Producer n) : *}]

@(noindent)In addition to requiring a non-negative constraint for @code{n},
this rule also restricts which terms may serve as type indices. Well-formed type
index terms include only addition, subtraction, and a few Video primitives. If
the @code{Producer} type constructor is applied to an unsupported term, the
type defaults to a @code{Producer} of infinite length. Despite these
restrictions, Typed Video works well in practice and can type check all our
example programs, including those for the RacketCon 2016 video proceedings.

The @sc{Lam} and @sc{App} rules in @figure-ref{lam-app-rules} roughly
illustrate how Typed Video handles constraints. Rather than implement multiple
passes, Typed Video interleaves constraint collection and type checking,
solving the constraints in a "local" manner.

Specifically, type checking judgements additionally specify a set of
constraints φ on the right-hand side, as in @figure-ref{lam-app-rules},
corresponding to the constraints produced while type checking that
expression. Constraints in Typed Video are restricted to linear arithmetic
inequalities.
The @sc{Lam} rule in @figure-ref{lam-app-rules} shows that functions bind a
type index variable @code{n} together with a procedural variable @code{x}, and must
satisfy input constraints φ. These functions are assigned a universally
quantified type that additionally includes constraints φ' collected while type
checking the body of the function. When applying such a function, the @sc{App}
rule first infers a concrete number @code{i} at which to instantiate the
@code{n} index variable. It then uses @code{i} to simplify constraints φ via a
partial-evaluation function @(make-element (make-style "overrightharp"
'(exact-chars)) @(make-element (make-style "ensuremath"
'(exact-chars)) (make-element (make-style "mathcal" '(exact-chars)) "E"))).
Type checking fails if the resulting constraints are not
satisfiable. Otherwise, the unsolved constraints are propagated. Modularly
checking constraints at function applications in this manner enables the
reporting of errors on a local basis.

@figure["lam-app-rules" @list{λ and function application type rules for Typed Video}]{
@centered[
@inferrule[#:name "Lam" "$\\Gamma$,n:Int,x:$\\tau\\vdash$ e : $\\tau'$; $\\phi'$"]{$\Gamma\vdash\lambda$n,x:$\tau$,$\phi$.e : $\Pi\{$n$\mid\phi\wedge\phi'\}.\tau\rightarrow\tau'$; true}
]
@vspace{10}
@centered[
@inferrule[#:name "App" "$\\Gamma\\vdash$ f : $\\Pi\\{$n$\\mid\\phi\\}.\\tau\\rightarrow\\tau';\\phi_1$"
                        "$\\exists$i.$\\Gamma\\vdash e : \\tau[n/i];\\phi_2$"
                        "\\overrightharp{$\\mathcal{E}$}$(\\phi[n/i]) = \\phi_3$, $\\phi_3$ satisfiable"]{$\Gamma\vdash$ f e : $\tau'[n/i];\phi_1\wedge\phi_2\wedge\phi_3$}
]
}

@; -----------------------------------------------------------------------------
@section[#:tag "type-implementation"]{Type Systems as Macros}

The implementation of Typed Video relies on linguistic reuse to produce a
full-fledged programming language without starting from scratch. Specifically,
it reuses Racket's syntax system to implement type checking, following
@nonbreaking{@cite-author[tsam-popl]'s [@cite-year[tsam-popl]]}
type-systems-as-macros technique. As a result, Typed Video is an extension to,
rather than a reimplementation of, the untyped Video language.

@Figure-ref{type-checking-macros} shows the implementation of two rules:
@racket[λ] and function application. The @racket[require] at the top of the
figure imports and prefixes the identifiers from untyped Video, i.e., the
syntactic extensions from @secref{implementation}, which are used, unmodified,
to construct the output of the type-checking pass.

We implement our type checker with Turnstile, a Racket DSL introduced by Chang
et al. for creating typed DSLs. This DSL-generating-DSL uses a concise,
bidirectional type-judgement-like syntax. In other words, the
@racket[define-syntax/typecheck] definitions in @figure-ref{type-checking-macros} resemble their specification counterparts in
@figure-ref{lam-app-rules}. The implementation rules define syntax transformers
that incorporate type checking as part of syntax elaboration. Interleaving type
checking and elaboration in this manner not only simplifies implementation of
the type system, but it also enables creating true abstractions on top of the
host language.

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

@; lambda rule explanations ------------------

@(current-line 4)

@with-linelabel{The transformer's input must match
@racket[(λ {n ...} ([x : τ] ... #:when φ) e)], a pattern that binds five  pattern variables:
@racket[n] (the type index variables), @racket[x] (the λ parameters), @racket[τ] (the
type annotations), @racket[φ] (a side-condition), and @racket[e] (the lambda
body).}


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
@#,line-no[](define-syntax/typecheck (λ {n ...} ([x : τ] ... #:when φ) e) ≫
@#,line-no[]  [(n ...) ([x ≫ x- : τ] ...) ⊢ e ≫ e- ⇒ τ_out]
@#,line-no[]  #:with new-φs (get-captured-φs e-)
@#,line-no[]  ------------------------------------------------------------
@#,line-no[]  [⊢ (untyped-video:λ (x- ...) e-)
@#,line-no[]      ⇒ (Π (n ...) #:when (and φ new-φs) (→ τ ... τ_out))])
@#,line-no[]
@#,line-no[](define-syntax/typecheck (#%app e_fn e_arg ...) ≫ 
@#,line-no[]   [⊢ e_fn ≫ e_fn- ⇒ (Π Xs #:when φX (→ τ_inX ... τ_outX))]
@#,line-no[]   #:with solved-τs (solve Xs (τ_inX ...) (e_arg ...))
@#,line-no[]   #:with (τ_in ... τ_out φ) (inst solved-τs Xs (τ_inX ... τ_outX φX))
@#,line-no[]   #:with φ* (type-eval φ)
@#,line-no[]   #:fail-unless (not (false? φ*)) "failed side-condition φ at row:col ..."
@#,line-no[]   #:unless (boolean? φ*) (add-φ φ*)
@#,line-no[]   [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
@#,line-no[]   ------------------------------------------------------------
@#,line-no[]   [⊢ (untyped-video:#%app e_fn- e_arg- ...) ⇒ τ_out])
]
}

@; if the readers don't understand this by now, we're lost
@;{Pattern variables may be used to construct new program
fragments, e.g., the outputs of the transformer. An ellipses pattern matches
zero-or-more of its preceding pattern; any pattern variables in that pattern
requires ellipses when used.}

@with-linelabel{Since type checking is interleaved with syntax elaboration,
Turnstile type judgements are elaboration judgements as well. Specifically, a
judgement of the form @racket[[ctx ⊢ e ≫ e- ⇒ τ]] is read ``in context
@racket[ctx], @racket[e] elaborates to @racket[e-] and has type @racket[τ].''

Thus the λ transformer elaborates @racket[e] to @racket[e-], simultaneously
computing its type @racket[τ_out]. This elaboration and type checking occurs in
the context of the free variables. Instead of propagating a type environment,
Turnstile reuses Racket's lexical scoping to implement the type
environment. This re-use greatly enhances the compositionality of languages and
reduces effort so that a programmer gets away with specifying only new
environment bindings. Specifically, the first premise adds type index variables and lambda parameters to the type environment, where the latter may contain references to the former.}

@with-linelabel{A @racket[#:with] premise binds additional pattern
variables. Here, elaborating the lambda body @racket[e] may generate additional
side-conditions, @racket[new-φs], that must be satisfied by the function's
inputs. Rather than thread the side-conditions through every subexpression, as
in @figure-ref{lam-app-rules}, Typed Video's implementation utilizes a separate
imperative interface for collecting constraints. Specifically, we use the
@racket[get-captured-φs] and @racket[add-φ] functions to propagate
side-conditions (the definition of these functions are not shown).}

@with-linelabel{These dashes separate the premises from the conclusion.}

@with-linelabel{The conclusion specifies the transformer's outputs: 
@racket[(untyped-video:λ (x- ...) e-)], an untyped term, along with its type
@racket[(Π (n ...) #:when (and φ new-φs) (→ τ ... τ_out))]. In Turnstile, types
are represented using the same syntax structures as terms.}

@exact{\vspace{0.4cm}}

The second part of @figure-ref{type-checking-macros} presents Typed Video's
function application rule implementation. It naturally interposes on Racket's
function application hook, @racket[#%app], via untyped Video's function
application definition, to add type checking. Here is a brief description of
each line of @racket[#%app]:

@(inc-line)
@(inc-line)

@; function application rule explanations -------------------

@with-linelabel{The input syntax is matched against pattern @racket[(#%app e_fn
e_arg ...)], binding the function to @racket[e_fn] and all arguments to
@racket[e_arg].}

@with-linelabel{The function elaborates to @racket[e_fn-] with type @racket[(Π
Xs #:when φX (→ τ_inX ... τ_outX))], which is universally quantified over type
index variables @racket[Xs] and has side-condition @racket[φX].}

@with-linelabel{The transformer peforms local type inference, computing the
concrete type indices @racket[solved-τs] at which to instantiate the polymorphic
function.}

@with-linelabel{Next, the polymorphic function type is instantiated to concrete
types @racket[(τ_in ... τ_out)] and side-condition @racket[φ].}

@;item{The side-condition @racket[C] is reduced to a canonical form @racket[φ*].}

@with-linelabel{The transformer partially evaluates the concrete constraints in @racket[φ], producing @racket[φ*].}

@with-linelabel{If @racket[φ*] is @racket[false], elaboration stops and the
transformer reports an error. Though this paper truncates the error message
details, this line demonstrates how our DSL creates true abstractions,
reporting errors in terms of the surface language rather
than the host language.}

@with-linelabel{If @racket[φ*] is still an expression, propagate it using
@racket[add-φ].}

@with-linelabel{Check that the function arguments have the instantiated
types. This premise uses the ``check'' left arrow. If a
programmer does not explicitly implement a left-arrow version of a rule,
Turnstile uses a standard subsumption rule by default.}

@(inc-line)

@with-linelabel{The generated code consists of an untyped Video term along
with its computed type.} 

@exact{\vspace{0.4cm}}

The rest of the implementation of the typing rules is similar. For example,
implementing @tt{Π} is straightforward because Turnstile reuses Racket's
knowledge of a program's binding structure to automatically handle
naming. Further, the ``as macros'' approach facilitates implementation of rules
for both terms and types, and thus the implementation of type-level
computations also resembles the rules in @figure-ref{type-checking-macros}.

