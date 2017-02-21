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

Using the Racket ecosystem and design allows developers to
implement languages quickly and easily. These languages
compose so that programs written in one language can easily
interact with programs in another. Furthermore, the
implementation of one language takes advantage of language
implemented before it. The upshot here is that implementing
Video is as simple as implementing domain-specific
components, and then integrating them into the existing
Racket ecosystem.

This section discusses how a developer can implement a DSL
in Racket (@secref["impl-create"]). In particular, this
section shows that creating a full working implementation
requires little effort. Video serves as an example of a DSL
that follows this design (@Secref["impl-video"]). Finally,
not only is Video implemented using the Racket doctrine, but
part of the implementation of Video is implemented using the
Racket doctrine. That is, Video is implemented in a DSL
designed specifically for implementing Video, which itself
is implemented in Racket (@secref{impl-ffi}).

@section[#:tag "impl-create"]{Creating Languages, the Racket Way}

Creating Racket DSLs is straightforward. Language authors simply
remove unwanted features from some base language, add new
features, and alter existing features to match desired
semantics.

Adding and removing features is simple because a language is
a module like any other module in Racket. Language authors
create a module that defines the new features, export those
features, and does not export unwanted ones. They do this in
the same manner as a programmer augmenting the functionality
of a library@cite[lal-pldi]. Unlike adding or removing
features, modifying existing features requires a little more
work than that. Specifically it @TODO{... explain ...}.

In addition to macro extensibility, Racket's metaprogramming
system supports several interposition points that facilitate
language creation: @racket[#%app] for function application,
@racket[#%module-begin] for modules, @racket[#%datum] for
literals, @racket[#%provide] for module exports, and so on.
The purpose of these points is to allow language authors to
alter the semantics of the relevant features to match the
desired language semantics.
As an example, a developer can
use a strict @racket[#%app] to turn a language with
call-by-values semantics into one with call-by-need
semantics. The @racketmodname[lazy/racket] language uses
this interposition point to convert @racketmodname[racket],
a language that employs strict semantics, into an otherwise
equivalent language with lazy semantics. The @racket[#%app]
protocol works because the Racket compiler places it in
front of every function application. Thus, language authors
only need to implement their version of @racket[#%app] in
terms of an existing one. Using the example from above, lazy
function application can be implemented as such:

@(nested (minipage @racketblock[
 (f a b c ...) @#,->text{compiles to} ((code:hilite @#,elem{internal @racket[#%app]}) f a b c ...)
               @#,->text{compiles to} (#%lazy-app f a b c ...)
               @#,->text{compiles to} (#%racket-app (force f) (lazy a) (lazy b) (lazy c) ...)]))

@section[#:tag "impl-video"]{The Essence of Video}

Video's implementation is spread across several main
components: a surface syntax, a core library, and a
rendering layer.
It consists of 3219
lines of code. Of that, about 95 lines are specific to the
surface syntax, and 353 lines define the video-specific
primitives the language uses. The latter 
are implemented using standard functional programming
techniques. They only serve as a thin wrapper of functions
to Video's core data-types. 

@; #%plain-lambda -> #%lambda-begin?

Surface syntax is where Video uses two of Racket's
interposition points, namely @racket[#%module-begin] and
@racket[#%plain-lambda]. These points enable developers to
define the semantics of the language's module and local level
scope respectively. As with @racket[#%app], using these
forms enables authors to reuse the syntax of an existing
language, while still being able to reinterpret both whole modules
and local pieces.

Replacing these forms is what enables the splicing of
expressions composing to form a larger Video, and even the lifted
@racket[define] forms. Similarly to @racket[#%app],
@racket[#%module-begin] gets placed at the start of every
module. Video needs to define its own
@racket[#%module-begin] to interpret change the module semantics.

@figure["mod-begin" "Compilation for a Video Module"]{
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  @racketmod0[
 video
 (image "splash.png" #,elided)
 (make-conf-video video #,elided)
 (define video #,elided)]
  (->text "compiles to")
  @racketblock0[
 ((code:hilite @#,elem{internal @racket[#%module-begin]})
  (image "splash.png" #,elided)
  (make-conf-video video #,elided)
  (define video #,elided))])
 
 @exact{\vspace{0.4cm}}

@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "compiles to")
  @racketblock0[
 (#%video-module-begin
  (image "splash.png" #,elided)
  (make-conf-video video #,elided)
  (define video #,elided))])

 @exact{\vspace{0.4cm}}
 
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "compiles to")
  @racketblock0[
 (#%racket-module-begin
  (define video #,elided)
  (video-begin vid
    (image "splash.png" #,elided)
    (make-conf-video video #,elided)))])
  
 @exact{\vspace{0.4cm}}
 
@(3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "compiles to")
  @racketblock0[
 (#%racket-module-begin
  (define video #,elided)
  (provide vid)
  (define vid
    (playlist
     (image "splash.png" #,elided)
     (make-conf-video video #,elided))))])
 @exact{\vspace{0.2cm}}}

@Figure-ref["mod-begin"] shows the compilation of a module
for a conference talk. Here, @racket[#%video-module-begin]
is Video's variant, while @racket[#%racket-module-begin] is
the standard one provided by @racketmodname[racket]. As
described informally in preceding sections,
@racket[#%video-module-begin] lifts the definitions to the
top of the module, and collects the remaining expressions
into one data-structure called @racket[video-begin]. This
data structure compiles into a playlist that is bound to
@racket[vid], and provides that playlist from the module.
The compilation for @racket[#%plain-lambda] also follows
this pattern.

@TODO{how do you know what a def vs expr is?}

@figure["video-begin" "Figure title"]{
@RACKETBLOCK[
 (define-syntax (video-begin stx)
   (syntax-parse stx
     [(_ id exprs)
      #`(begin
          (define id (playlist . #,(reverse (syntax->list #'exprs))))
          (provide id))]
     [(_ id exprs b1 . body)
      (define expanded (local-expand #'b1 'module (UNSYNTAX elided)))
      (syntax-parse expanded
        [(id* . rest)
         #:when (and (identifier? #'id*)
                     (or (free-identifier=? #'id* provide)
                         (free-identifier=? #'id* define-values)
                         (UNSYNTAX elided)))
         #`(begin #,expanded (video-begin id post-process exprs . body))]
        [_
         #`(video-begin id (#,expanded . exprs) . body)])]))]}

@section[#:tag "impl-ffi"]{Video, Behind the Scenes}

Rather than rendering files directly, Video employs a C
library---the MLT Multimedia Framework@note[mlt-url]---to
perform the actual rendering. 

As it turns out, the Racket design pattern actually makes
the step of adding bindings trivial. That is to say, when
the task of importing bindings manually is daunting, simply
make a DSL to do it for you. For example, Video employs a
one-off DSL just for building its internal data structures.
The implementation of this DSL is small but reduces so
much code, so that the entire implementation of the DSL in
combination with the actual back-end of Video is smaller
than an otherwise identical back-end that does not use a
DSL. Thus, creating the DSL reduces enough effort that it
offsets the cost of creating it, even though it is used for
only one program.

Specifically, this DSL provides two forms:
@racket[define-mlt] and @racket[define-constructor]. The
first form introduces a small DSL that combines with the
Racket FFI to make importing Racket bindings trivial. While
the second form is more complex, the premise is simple,
@racket[define-constructor] defines the core data types for
Video. Additionally, this form cooperates with the renderer
to convert these data types into data that MLT understands.
