#lang scribble/sigplan

@require[(except-in scribble/manual cite)
         scriblib/figure
         scriblib/footnote
         (except-in scribble/core paragraph table)
         pict
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "implementation"]{The Fast and the Furious}

Using the Racket ecosystem allows developers to implement
languages quickly and easily. Furthermore, these languages
compose so that modules written in one language can easily
interact with modules in another. Best of all, the
implementation of a language may take advantage of other
language technology too. The upshot here is that implementing Video is as
simple as implementing video-specific pieces, while leaving
the general-purpose bits to Racket.

This section explains how a developer can implement a DSL
in Racket (@secref["impl-create"]), with Video serving as
the running example (@secref["impl-video"]). Not only is
Video a Racket DSL, but part of the implementation of Video
is implemented using another DSL, one designed specifically
for implementing Video (@secref["impl-ffi"]).

@section[#:tag "impl-create"]{Creating Languages, the Racket Way}

Resuming the train of thought from @secref["rationale"],
creating Racket DSLs means removing unwanted features
from some base language, adding new ones, and altering existing
features to match the semantics of the new domain.

Adding and removing features is simple, because a language
implementation is a module like any other module in Racket.
A language developer creates a module that defines the new
features, exports those features, and does not export unwanted
ones. They do so in the same manner as a programmer who 
augments the functionality of a library via a wrapper
module. In contrast, modifying existing features requires a
little more work than that. Specifically the module must
define a syntax transformation in terms of the old language,
using a distinct name, and rename that primitive on export.

@figure["racket-boo" @list{Logging assignment statements to expose
 ill-behaved functional programmers}]{
@racketmod[
 racket/base

 (provide (rename-out boo:set! set!)
          (except-out (all-from-out racket/base) set!))

 (define-syntax (boo:set! stx)
   (syntax-parse stx
     [(_ id val)
      #'(begin (log-warning "Boo!!! Reassigning ~a" id)
               (set! id val))]))]
}

Let's illustrate this idea with a simple example. Functional programmers do
not like assignment statements, and at Clojure conferences programmers who
admit to their use (via Java) are booed on stage.@note{Clojure has the largest, most
successful community of commercial functional programmers. One of the
authors witnessed this booing at a recent Clojure conference.} So, imagine
creating a language like Racket that logs a boo-warning of any use of
@racket[set], Racket's variable reassignment form. The @racket[set!] provided
by @racketmodname[racket/base] provides the functionality for reassignment,
while @racket[log-warning] from the same language provides the logging
functionality. All we have to do is define a new syntax transformer, say   
@racket[boo:set!], that logs its use and performs the
assignment. From there, we need to rename @racket[boo:set!]
to @racket[set!] in the @racket[provide] specification. This renaming
makes the revised @racket[set!] functionality available to programmers who use the new and
improved Functions-first variant of Racket. @Figure-ref{racket-boo} displays the complete
solution. 

@figure["lazy-racket" @list{An essential element of Lazy Racket}]{
@(minipage @racketmod[
 racket/base

 (provide (rename-out #%lazy-app #%app)
          (excpet-out (all-from-out racket/base) #%app))

 (define-syntax (#%lazy-app stx)
   (syntax-parse stx
     [(_ rater rand ...)
      #'(#%app (force rater) (lazy rand)  ...)]))])
}

Also recall that Racket's syntax system supports several interposition
points that facilitate language creation: @racket[#%app] for function
application, @racket[#%module-begin] for module boundaries,
@racket[#%datum] for literals, and so on.  The purpose of these points is
to allow language developers to alter the semantics of the relevant
features without changing the surface syntax.

For example, a language developer can use a strict @racket[#%app] to
construct a lazy form of function application. Indeed, the Racket family of
languages comes with the @racketmodname[lazy/racket] language@cite[bc:lazy], which uses
exactly this interposition point to convert @racketmodname[racket] into an
otherwise equivalent language with lazy semantics. The @racket[#%app]
protocol works because the Racket compiler places the marker in front of
every function application. Thus, language developers only need to
implement their version of @racket[#%app] in terms of an existing one. See
@figure-ref{lazy-racket} for the concrete definition.

When a programmer uses this new language, the Racket syntax elaborator
inserts @racket[#%app] into all regular function applications. The
elaborator simultaneously resolves this reference to the imported version, 
written as @racket[#%app]@superscript{lazy}. From there, Racket
resolves the renaming and expands @racket[#%lazy-app] into
@racket[#%app]@superscript{base}, Racket's actual function
application. Here is what the complete elaboration process looks like:
@;
@nested[#:style 'vertical-inset]{
@racketblock[
 (f a b c ...) @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{lazy}} f a b c ...)
               @#,->text{elaborates to} (#%lazy-app f a b c ...)
               @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{base}} (force f) (lazy a) (lazy b) (lazy c) ...)

]
}
@; 
The curious reader may wish to step through the elaboration via DrRacket's
syntax debugger@cite[culpepper-scp].

Video's implementation consists of three major components and accounts for
approximately 2,400 lines of code: a syntax, a run-time library, and a
rendering layer. Of the code, about 90 lines are specific to the syntax and
350 lines define the video-specific primitives the language uses. The
latter serve as functional adapters of imperative functions that work on
Video's core data-types; they are implemented using standard functional
programming techniques.  The following sub-section covers the
implementation of Video's syntax, and the third one presents the
implementation of the renderers. 

@; -----------------------------------------------------------------------------
@section[#:tag "impl-video"]{The Essence of Video's Syntax}

The syntax implementation of Video uses two of Racket's interposition
points, namely @racket[#%module-begin] and @racket[#%plain-lambda]. These
points define the lexical scope of modules and functions. As with
@racket[#%app], using these forms enables language developers to
simultaneously reuse Racket syntax and re-interpret it. 

Similar to @racket[#%app], @racket[#%module-begin] gets
inserted at the start of every module and wraps the entire
contents of that module. Hence, a re-interpretation may 
easily implement context-sensitive transformations. In the case of Video,
the re-implementation of @racket[#%module-begin] lifts definitions to the
beginning of the module and collects the remaining expressions into a
playlist. 

@(define *line-no 0)
@(define (line-no)
   (set! *line-no  (+ *line-no 1))
   (define line-no (format (if (< *line-no 10) "0~a" "~a") *line-no))
   @exact{\tt @line-no})

@figure["video-begin" "Video Compilation"]{
@racketmod[
#:escape L
racket/base

(L @line-no[])   (provide (rename-out [#%video-module-begin module-begin])
(L @line-no[])            (except-out #%module-begin racket/base))
(L @line-no[])  
(L @line-no[])   (define-syntax (#%video-module-begin stx)
(L @line-no[])     (syntax-parse stx
(L @line-no[])       [(_ body ...)
(L @line-no[])        #'(#%module-begin (video-begin vid (provide vid) () body ...))]))
(L @line-no[])  
(L @line-no[])  (define-syntax (video-begin stx)
(L @line-no[])    (syntax-parse stx
(L @line-no[])      [(video-begin vid code exprs)
(L @line-no[])       #`(begin
(L @line-no[])           (define vid (playlist . #,(reverse (syntax->list #'exprs))))
(L @line-no[])           code)]
(L @line-no[])      [(video-begin vid code exprs b1 body ...)
(L @line-no[])       (define expanded (local-expand #'b1 'module (L elided)))
(L @line-no[])       (syntax-parse expanded
(L @line-no[])         [(id*:id rest ...)
(L @line-no[])          #:when (matches-one-of? #'id* (list #'provide #'define (L elided)))
(L @line-no[])          #`(begin #,expanded (video-begin vid code body ...))]
(L @line-no[])         [else
(L @line-no[])          #`(video-begin id (#,expanded . exprs) . body)])]))
]
}

@Figure-ref["video-begin"] shows the essence of the
@racket[#%module-begin] syntax transformer. It is written in Racket's
@racket[syntax-parse] language@cite[fortifying-jfp], a vast improvement over
Kohlbecker et al's Scheme macros@cite[kffd:hygiene kw:mbe]. As
before, the transformer is defined with a different name,
@racket[#%video-module-begin] (line 4), and is renamed on
export (line 1). The implementation of
@racket[#%video-module-begin] dispatches to
@racket[video-begin] (line 7), the workhorse of the module. This auxiliary syntax
transformer consumes four pieces: an identifier (@racket[vid]), a piece of
@racket[code] formulated in terms of the identifier, a list of expressions,
and the module's body, which is represented as a potentially empty sequence
of expressions. In the case of @racket[#%video-module-begin], the four
pieces are @racket[vid], @racket[(provide vid)], @racket[()], and the module
body. 

The @racket[video-begin] syntax transformer (lines 9--22) is responsible
for lifting definitions to the beginning of the module body and
accumulating expressions into a playlist. The definition for
@racket[video-begin] uses pattern matching again. Lines 11 and 15 specify
the two pattern cases, one for when the module body is
empty and another one that handles a non-empty sequence of body
expressions:
@;
@itemlist[

@item{Once @racket[video-begin] has traversed every piece of syntax (line 11), 
@racket[exprs] contains all of the original module body's expressions in
reverse order. The generated output (lines 12-14) defines the
given @racket[vid] to stand for the list of expressions
bundled as a playlist.  The last piece of the @racket[begin] block is
@racket[code].}

@item{In the second case, the transformer expands the first term
up to the point where it can decide whether it is a definition (line 16).  
Next, the transformer uses @racket[syntax-parse] to  check whether the
elaborated code is a syntax list (lines 18 and 20) with a recognized
identifier in the first position (lines 18--19), e.g., @racket[define] and
@racket[provide].  

If so, the transformer lifts the first term out of the @racket[video-begin]
and recursively calls itself without the newly lifted expression (line 20).

Otherwise, it is an expression and gets collected into the @racket[exprs]
collection (line 22).}

]
The astute reader may wonder about the generated @racket[begin] blocks. As
it turns out, Racket's @racket[#%module-begin] flattens @racket[begin]
sequences at the module top-level into a simple sequence of terms. 

The syntax transformer for function bodies also uses @racket[video-begin]. 
Instead of handing over @racket[(provide vid)], the call in the function
transformer merely passes along @racket[vid], because functions return the
produced playlist, they do not export it. 

@Figure-ref["mod-begin"] shows the syntax elaboration of a
module using the Video specific @racket[#%module-begin]
form. The elaborated module describes the running conference
talk example. Here, @racket[#%module-begin]@superscript{
 video} is Video's variant, while
@racket[#%module-begin]@superscript{base} is the one
provided by @racketmodname[racket].

@figure["mod-begin" @list{Compilation for a Video module}]{
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  @racketmod0[
 video
 (image "splash.png" #,elided)
 (conference-talk video #,elided)
 (define video #,elided)]
  (->text "elaborates to")
  @racketmod0[
 video
 (@#,elem{@racket[#%module-begin]@superscript{video}}
  (image "splash.png" #,elided)
  (conference-talk video #,elided)
  (define video #,elided))])
 
 @exact{\vspace{0.4cm}}

@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "elaborates to")
  @racketmod0[
 racket/base
 (#%video-module-begin
  (image "splash.png" #,elided)
  (conference-talk video #,elided)
  (define video #,elided))])

 @exact{\vspace{0.4cm}}
 
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "elaborates to")
  @racketmod0[
 racket/base
 (@#,elem{@racket[#%module-begin]@superscript{base}}
  (define video #,elided)
  (video-begin vid
    (image "splash.png" #,elided)
    (conference-video video #,elided)))])
  
 @exact{\vspace{0.4cm}}
 
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "elaborates to")
  @racketmod0[
 racket/base
 (@#,elem{@racket[#%module-begin]@superscript{base}}
  (define video #,elided)
  (provide vid)
  (define vid
    (playlist
     (image "splash.png" #,elided)
     (conference-video video #,elided))))])
 @exact{\vspace{0.2cm}}}


@; -----------------------------------------------------------------------------
@section[#:tag "impl-ffi"]{Video, Behind the Scenes}

Video relies on bindings to a C library---the MLT Multimedia Framework---to
perform the rendering of video descriptions to files or streams. It
exploits Racket's FFI language for this purpose@cite[bo:ffi].

As it turns out, the Racket doctrine actually applies to the step of adding
bindings too. That is to say, because the task of importing bindings
manually is daunting, the implementation uses a DSL for just this task. It
turns out that, once again, the effort of implementing this auxiliary DSL
plus writing a single program in it is smaller than the effort of just
writing down the FFI bindings. In short, creating the DSL sufficiently
reduces the overall effort that it offsets the startup cost, even though it
is used @emph{only once}.

The auxiliary DSL relies on two forms: @racket[define-mlt] and
@racket[define-constructor]. The first form uses the Racket FFI to import
of bindings from MLT.  The second form, @racket[define-constructor],
defines the core data types for Video and sets up a mapping to data that
MLT understands.

The @racket[define-mlt] form is useful for hardening foreign
functions. By using @racket[define-mlt],
programmers must only specify a
contract@cite[contracts-icfp] that describes the valid inputs
and outputs. Consider @tt{mlt_profile_init}, a
function that initializes a C library.
It takes a string and returns either a
profile object or @tt{NULL} if there is an error. Rather
than having to manually check the input and output values,
the FFI just states input and output types:
@;
@nested[#:style 'vertical-inset]{
@racketblock[
 (define-mlt mlt-profile-init (_fun _string
                                    -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))
]
}
@;
It errors if a bad input or output type passes through this interface.

The @racket[define-constructor] both introduces structures to represent
 video clips in Video and methods for converting these Video-level objects
 into structures that MLT understands. For its first purpose, it
 generalizes Racket's @racket[struct] definitions, that is, it optionally
 specifies a super-struct, additional fields, and their default values. 
 For example, the following is the description of a Video-level producer:
@;
@nested[#:style 'vertical-inset]{
@racketblock[
 (define-constructor producer service ([type #f] [source #f] [start -1] [end -1])
   (define producer* (mlt-factory-producer (current-profile) type source))
   (mlt-producer-set-in-and-out producer* start end)
   (register-mlt-close mlt-producer-close producer*))
]
}
@;
 This definition names the struct @racket[producer], specifies that it
 extends @racket[service], and adds four fields to those it inherits:
 @racket[type], @racket[source], @racket[start], and @racket[end]. 

For its second purpose, @racket[define-constructor] introduces the body of
 a conversion function, which renderers know about. Here the function body
 consists of three lines. It has access to all of the structure's fields,
 as well as the parent stucture's fields. The renderer calls this
 conversion code at the point when it needs to operate over MLT objects
 rather than Video ones.

The implementation of this Video FFI adds nothing to the ideas of the
 previous section, which is why its details are omitted. 
