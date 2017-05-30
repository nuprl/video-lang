#lang scribble/acmart

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
language technology, too. The upshot here is that implementing Video is as
simple as implementing video-specific pieces, while leaving
the general-purpose bits to Racket.

This section explains how a developer can implement a DSL
in Racket (@secref["impl-create"]), with Video serving as
the running example (@secref["impl-video"]). Not only is
Video a Racket DSL, but part of the implementation of Video
is implemented using additional DSLs designed specifically
for implementing Video (@secref["impl-ffi"]).

@section[#:tag "impl-create"]{Creating Languages, the Racket Way}

Resuming the train of thought from @secref["rationale"],
creating Racket DSLs means removing unwanted features
from some base language, adding new ones, and altering existing
features to match the semantics of the new domain.

Adding and removing features is simple, because a language
implementation is a module like any other module in Racket.
Removing features is simply a matter of not re-providing them
from the host language. Line 4 uses the @racket[except-out]
keyword to remove the definition of @racket[set!] from
@racketmodname[racket/base]. Also on line 4,
@racket[all-from-out] re-exports all of the features in
@racketmodname[racket/base].

@(define *line-no 0)
@(define (line-no)
   (set! *line-no  (+ *line-no 1))
   (define line-no (format (if (< *line-no 10) "0~a" "~a") *line-no))
   @exact{\tt @line-no})
@figure["racket-boo" @list{Logging assignment statements to expose
 ill-behaved functional programmers}]{
@racketblock[
@#,line-no[] @#,hash-lang[] racket/base
@#,line-no[]
@#,line-no[] (provide (rename-out boo:set! set!)
@#,line-no[]          (except-out (all-from-out racket/base) set!))
@#,line-no[]
@#,line-no[] (define-syntax (boo:set! stx)
@#,line-no[]   (syntax-parse stx
@#,line-no[]     [(_ id val)
@#,line-no[]      #'(begin (log-warning "Boo!!! Reassigning ~a" id)
@#,line-no[]               (set! id val))]))]
}

In addition to these operations, adding new features is simply a matter of
defining the new features and exporting them.
Developers do so in the same manner as a programmer who augments the
functionality of a library via a wrapper module.

In contrast, modifying existing features requires
work. Specifically the module must define a syntax
transformation in terms of the old language
and rename this definition on export.

Let us illustrate this idea with a simple example. Functional programmers do
not like assignment statements, and at Clojure conferences programmers who
admit to their use (via Java) are booed on stage.@note{Clojure has the largest, most
successful community of commercial functional programmers. One of the
authors witnessed this booing at a recent Clojure conference.} So, imagine
creating a language like Racket that logs a boo-warning of any use of
@racket[set!], Racket's variable reassignment form. The @racket[set!] provided
by @racketmodname[racket/base] provides the functionality for reassignment,
while @racket[log-warning] from the same language provides the logging
functionality. All we have to do is define a new syntax transformer, say   
@racket[boo:set!], that logs its use and performs the
assignment. From there, we need to rename @racket[boo:set!]
to @racket[set!] in the @racket[provide] specification. This renaming
makes the revised @racket[set!] functionality available to programmers who use the new and
improved Functions-first variant of Racket. @Figure-ref{racket-boo} displays the complete
solution. 

@(set! *line-no 0)
@figure["lazy-racket" @list{An essential element of Lazy Racket}]{
@(minipage @racketblock[
@#,line-no[] @#,hash-lang[] racket/base
@#,line-no[]
@#,line-no[] (provide (rename-out #%lazy-app #%app)
@#,line-no[]          (except-out (all-from-out racket/base) #%app))
@#,line-no[]
@#,line-no[] (define-syntax (#%lazy-app stx)
@#,line-no[]   (syntax-parse stx
@#,line-no[]     [(_ rator rand ...)
@#,line-no[]      #'(#%app (force rator) (lazy rand)  ...)]))])
 @exact{\vspace{0.1em}}
}

Now recall that Racket's syntax system supports several interposition
points that facilitate language creation: @racket[#%app] for function
application, @racket[#%module-begin] for module boundaries,
@racket[#%datum] for literals, and so on.  The purpose of these points is
to allow language developers to alter the semantics of the relevant
features without changing the surface syntax.

@Figure-ref["lazy-racket"] displays a small, illustrative
example. Here, a language developer uses a strict
@racket[#%app] to construct a lazy form of function
application. The @racket[#%app] protocol works because the
Racket compiler places the marker in front of every function
application. Thus, language developers only need to
implement their version of @racket[#%app] in terms of an
existing one. @note{Indeed, the Racket family of languages
 comes with the @racketmodname[lazy] language@cite[bc:lazy],
 which uses exactly this interposition point to convert
 @racketmodname[racket] into an otherwise equivalent language
 with lazy semantics.}

When a programmer uses this new language, the Racket syntax elaborator
inserts @racket[#%app] into all regular function applications. The
elaborator resolves this reference to the imported version, 
written as @racket[#%app]@superscript{lazy}. From there, Racket
redirects to @racket[#%lazy-app], which expands into
@racket[#%app]@superscript{base}, Racket's actual
application. Here is what the complete process looks like:
@;
@split-minipage[
 #:split-location 0.95
@nested[#:style 'vertical-inset]{
@racketblock[
 (f a b c ...) @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{lazy}} f a b c ...)
               @#,->text{elaborates to} (#%lazy-app f a b c ...)
               @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{base}} (force f) (lazy a) (lazy b) (lazy c) ...)

]
}
 (blank 1 1)]
@; 
The curious reader may wish to step through the elaboration via DrRacket's
syntax debugger@cite[culpepper-scp].

Video's implementation consists of three major components and accounts for
approximately 2,400 lines of code: a surface syntax, a run-time library, and a
rendering layer. Of the code, about 90 lines are specific to the syntax and
350 lines define the video-specific primitives the language uses. The remaining
lines are for the FFI and renderer.
The video-specific primitives serve as adapters to imperative actions that work on
Video's core data-types; they are implemented using standard functional
programming techniques.

@; -----------------------------------------------------------------------------
@section[#:tag "impl-video"]{The Essence of Video's Syntax}

The implementation of Video's syntax uses two of Racket's
interposition points: @racket[#%module-begin] and
@racket[#%plain-lambda]. With these forms, language
developers can simultaneously reuse Racket syntax and
interpret it in a Video-appropriate manner.

Like @racket[#%app], @racket[#%module-begin] is
inserted at the start of every module and wraps the
contents of that module. Hence, a re-interpretation may 
easily implement context-sensitive transformations. In the case of Video,
the re-implementation of @racket[#%module-begin] lifts definitions to the
beginning of the module and collects the remaining expressions into a
single playlist. 

@(set! *line-no 0)
@figure["video-begin" "Video Compilation"]{
@racketblock[
#:escape L
(L @line-no[]) (L @hash-lang[]) racket/base
(L @line-no[])
(L @line-no[]) (provide (rename-out [#%video-module-begin module-begin])
(L @line-no[])          (except-out #%module-begin racket/base))
(L @line-no[])  
(L @line-no[]) (define-syntax (#%video-module-begin stx)
(L @line-no[])   (syntax-parse stx
(L @line-no[])     [(#%video-module-begin body ...)
(L @line-no[])      #'(#%module-begin (video-begin vid (provide vid) () body ...))]))
(L @line-no[])  
(L @line-no[]) (define-syntax (video-begin stx)
(L @line-no[])   (syntax-parse stx
(L @line-no[])     [(video-begin vid export (exprs ...))
(L @line-no[])      #'(begin
(L @line-no[])          (define vid (playlist exprs ...))
(L @line-no[])          export)]
(L @line-no[])     [(video-begin vid export (exprs ...) b1 body ...)
(L @line-no[])      (syntax-parse (local-expand #'b1 'module (L elided)) (code:comment "<-- this-syntax")
(L @line-no[])        [(id*:id rest ...)
(L @line-no[])         #:when (matches-one-of? #'id* (list #'provide #'define (L elided)))
(L @line-no[])         #'(begin this-syntax (video-begin vid export body ...))]
(L @line-no[])        [else
(L @line-no[])         #'(video-begin id export (exprs ... this-syntax) body ...)])]))
]
}

@Figure-ref["video-begin"] shows the essence of Video's
@racket[#%module-begin] syntax transformer. It is written in Racket's
@racket[syntax-parse] language@cite[fortifying-jfp], a vast improvement over the
Scheme macro system@cite[kffd:hygiene kw:mbe hygenic-lisp closures-lfp syntax-lfp]. As
before, the transformer is defined with a different name,
@racket[#%video-module-begin] (line 6), and is renamed on
export (line 3). The implementation of
@racket[#%video-module-begin] dispatches to
@racket[video-begin] (line 9), the workhorse of the module. This auxiliary syntax
transformer consumes four pieces: an identifier (@racket[vid]), a piece of code
(@racket[export]) formulated in terms of the identifier, a list of expressions (@racket[e ...]),
and the module's body, which is represented as a sequence
of expressions (@racket[body ...]). In the case of @racket[#%video-module-begin], the four
pieces are @racket[vid], @racket[(provide vid)], @racket[()], and the module
body. 

The @racket[video-begin] syntax transformer (lines 11--24) is responsible
for lifting definitions to the beginning of the module body and
accumulating expressions into a @racket[playlist]. Its definition
uses pattern matching again. Lines 13 and 17 specify
the two pattern cases, one for when the module body is
empty and another one that handles a non-empty sequence of body
expressions:
@;
@itemlist[

@item{Once @racket[video-begin] has traversed every piece of syntax (line 13), 
@racket[exprs] contains all of the original module body's expressions in
reverse order. The generated output (lines 14--16) defines the
given @racket[vid] to stand for the list of expressions
bundled as a playlist.}

@item{In the second case, the transformer expands the first term
up to the point where it can decide whether it is a definition (line 18).
Next, the transformer uses @racket[syntax-parse] to check whether the
elaborated code is a syntax list (lines 19 and 22) with a recognized
identifier in the first position (line 21), e.g., @racket[define] and
@racket[provide].

@itemlist[
 @item{
If so, the transformer lifts the first term out of the @racket[video-begin]
and recursively calls itself without the newly lifted expression (line 21).}
@item{
Otherwise, it is an expression and gets collected into the @racket[exprs]
collection (line 23).}]}

]
The astute reader may wonder about the generated @racket[begin] blocks. As
it turns out, Racket's @racket[#%module-begin] flattens @racket[begin]
sequences at the module top-level into a simple sequence of terms. 

The syntax transformer for function bodies also uses @racket[video-begin]. 
Instead of handing over @racket[(provide vid)], the call in the function
transformer merely passes along @racket[vid], because functions @emph{return} the
produced @racket[playlist], they do not export it. 

@Figure-ref["mod-begin"] shows the syntax elaboration of a
module using the Video specific @racket[#%module-begin]
form. The elaborated module describes the running conference
talk example. Here, @racket[#%module-begin]@superscript{
 video} is Video's variant, while
@racket[#%module-begin]@superscript{base} is the one
provided by @racketmodname[racket].

@figure["mod-begin" @list{Compilation for a Video module}]{
 @exact{\begin{footnotesize}}
@(3split-minipage
  #:size-a 0.35
  #:size-b 0.17
  #:size-c 0.48
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
  #:size-a 0.35
  #:size-b 0.17
  #:size-c 0.48
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
  #:size-a 0.35
  #:size-b 0.17
  #:size-c 0.48
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
  #:size-a 0.35
  #:size-b 0.17
  #:size-c 0.48
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
 @exact{\vspace{0.2cm}}
 @exact{\end{footnotesize}}}

@; -----------------------------------------------------------------------------
@section[#:tag "impl-ffi"]{Video, Behind the Scenes}

Video relies on bindings to a C library to perform the
rendering of video descriptions to files or streams. It
exploits Racket's FFI language for this
purpose@cite[bo:ffi]. While Video uses the MLT Multimedia
Framework, any suitable multimedia library will do.

As it turns out, the Racket doctrine actually applies to the
step of adding bindings too. The task of importing bindings
manually is highly repetitive and developers frequently turn
to other tools or libraries to construct the FFI bindings
for them. Using a DSL to create the bindings has two
advantages over using a library for a similar task. First, a
DSL separates the task of constructing safe FFI calls with
the task of connecting to a specific multimedia library.
Using an FFI, developers can connect to a library safely by
specifying the contracts. Second, a DSL allows developers to
create new binding forms that are relevant to the multimedia
library. It turns out that, once again, the effort of
implementing this auxiliary DSL plus writing a single
program in it is smaller than the effort of just writing
down the FFI bindings directly. In other words, creating the
DSL sufficiently reduces the overall effort that it offsets
the startup cost, even though it is used @emph{only once}.

The auxiliary DSL relies on two key forms: @racket[define-mlt] and
@racket[define-constructor]. The first form uses the Racket FFI to import
bindings from MLT.  The second form, @racket[define-constructor],
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
@split-minipage[
 #:split-location 0.99
@nested[#:style 'vertical-inset]{
@racketblock[
 (define-mlt mlt-profile-init (_fun _string
                                    -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))
]
}
 (blank 1 1)]
@;
It errors if a bad input or output type passes through this interface.

The @racket[define-constructor] both introduces structures to represent
 video clips in Video and methods for converting these Video-level objects
 into structures that MLT understands. For its @emph{first} purpose, it
 generalizes Racket's @racket[struct] definitions with
 an optional super-struct, additional fields, and their default values. 
 For example, the following is the description of a Video-level producer:
@;
@split-minipage[
 #:split-location 0.99
@nested[#:style 'vertical-inset]{
@racketblock[
 (define-constructor producer service ([type #f] [source #f] [start -1] [end -1])
   (define producer* (mlt-factory-producer (current-profile) type source))
   (mlt-producer-set-in-and-out producer* start end)
   (register-mlt-close mlt-producer-close producer*))
]
}
 (blank 1 1)]
@;
 This definition names the struct @racket[producer], specifies that it
 extends @racket[service], and adds four fields to those it inherits:
 @racket[type], @racket[source], @racket[start], and @racket[end]. 

For its @emph{second} purpose, @racket[define-constructor] introduces the body of
 a conversion function, which renderers know about. Here the function body
 consists of three lines. It has access to all of the structure's fields,
 as well as the parent stucture's fields. The renderer calls this
 conversion code at the point when it needs to operate over MLT objects
 rather than Video ones.

Otherwise, the implementation of this Video FFI reiterates the same ideas
from @secref["impl-video"], and thus the details are omitted.
