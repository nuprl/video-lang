#lang scribble/sigplan

@require[(except-in scribble/manual cite)
         scriblib/figure
         scriblib/footnote
         (except-in scribble/core paragraph table)
         pict
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "implementation"]{The Fast and the Furious}

Using the Racket ecosystem and design allows developers to
implement languages quickly and easily. Furthermore, these
languages compose, meaning that implementing a language like
Video is as simple as implementing video-specific
components, and then integrating them into the existing
Racket ecosystem.

This section discusses how a developer can implement a DSL
following the Racket design (@secref["impl-create"]). It
shows that going from a design to a prototype, to a full
working implementation requires little effort. Video serves
as an example of a DSL that follows this design.
@Secref["impl-video"] shows how Video goes from a design to
a full implementation. Finally, not only is Video
implemented using the Racket doctrine, but the
implementation of Video is implemented using the Racket
doctrine. That is to say, Video is implemented in a DSL
designed specifically for implementing Video, which itself
is implemented in Racket (@secref{impl-ffi}).

@section[#:tag "impl-create"]{Creating Languages - The Racket Way}

Creating DSLs following the Racket doctrine is straight
forward and requires little effort. Language authors simply
remove unwanted features from Racket, add new desired
features, and alter existing features to match desired
semantics.

Following the Racket design, adding and removing features is
straightforward. Authors create a module that defines the
new features, and does not export unwanted ones. They do
this in the same manner as a programmer augmenting the
functionality of a library@cite[lal-pldi]. Unlike adding or
removing features, modifying existing features is less
straightforward. However, following the Racket dogma makes
creating modified features easy.

Racket's metaprogramming system contains several
interposition points that facilitate language creation. The
purpose of these points is to allow language authors to
alter the semantics of the relevant features to match the
desired language semantics.

One such location, @racket[#%app], is useful for changing
the meaning of function application in a language. A
developer can use @racket[#%app] to turn a language with
call-by-values semantics to one with call-by-need semantics.
The @racketmodname[lazy/racket] language uses this
interposition point to convert @racketmodname[racket], a
language that employs strict semantics, into an
otherwise equivalent language with lazy semantics.

@(compound-paragraph
  (style #f '())
  (list
   @para{The @racket[#%app] protocol works because the
 Racket compiler places it in front of every function
 application. Thus, language authors only need to implement
 their version of @racket[#%app] in terms of an existing one.
 Using the example from above, lazy function application can
 be implemented as such:}

   (nested (minipage @racketblock[
 (f a b c ...) @#,->text{compiles to} (#%lazy-app f a b c ...)
               @#,->text{compiles to} (#%app (force f) (lazy a) (lazy b) (lazy c) ...)]))
   
   @para{Here, @racket[#%app] is @racketmodname[racket]'s
 function application form, while @racket[#%lazy-app] is a
 lazy variant.}))
  
@section[#:tag "impl-video"]{Video Editing as a Language}

Video's implementation is spread across several main
components: a surface syntax, a core library, and a
rendering layer. Racket's meta-programming
facilities@cite[macros-icfp], specifically the interposition
points discussed above, play a major role in the
implementation.

This language also serves as an example of the power and
ease of use the Racket design. Video is implemented in 3219
lines of code. Of that, about 95 lines are specific to the
surface syntax, and 353 lines define the video-specific
primitives the language uses. The video-specific primitives
are implemented using standard functional programming
techniques. They only serve as a thin wrapper of functions
to Video's core data-types. 

@; #%plain-lambda -> #%lambda-begin?

Surface syntax is where Video makes use of Racket's
interposition points, namely @racket[#%module-begin] and
@racket[#%plain-lambda]. These points enable creators to
alter the semantics of the language's module and local level
scope respectively. As with @racket[#%app], using these
forms enables authors to reuse the facilities of an existing
language, while still being able to make both whole program
and local modifications. This differs from many other DSLs
that are written independently of any host language.
Normally, the syntax for these languages are cobbled
together using either low-level parsing tools or by directly
modifying the code that implements of an existing language.
The former option makes writing a robust language difficult,
while the latter option removes any possibility of composing
languages written in a similar fashion.

Replacing these forms is what enables the splicing of
expressions composing to form a larger Video, and even the
@racket[define*] form. Similarly to @racket[#%app], and
@racket[#%module-begin] gets placed at the start of every
module. Video simply needs to define its own
@racket[#%module-begin] to change the module semantics. The
following is a version of the conference talk module (that
elides several streams and require forms), as Video compiles
it:

@(minipage
 (3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  @racketmod[
 video
 (image "splash.png" ...)
 (make-conf-video video ...)
 (define video ...)]
  (->text "compiles to")
  @racketblock0[
 (#%video-module-begin
  (image "splash.png" ...)
  (make-conf-video video ...)
  (define video ...))])

 @exact{\vspace{0.2cm}}
 
 (3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "compiles to")
  @racketblock0[
 (#%module-begin
  (define video ...)
  (video-begin vid
    (image "splash.png" ...)
    (make-conf-video video ...)))])
  
 @exact{\vspace{0.2cm}}
 
 (3split-minipage
  #:size-a 0.38
  #:size-b 0.22
  #:size-c 0.40
  (blank)
  (->text "compiles to")
  @racketblock0[
 (#%module-begin
  (define video ...)
  (provide vid)
  (define vid
    (playlist
     (image "splash.png" ...)
     (make-conf-video video ...))))]))

Here, @racket[#%video-module-begin] is Video's variant,
while @racket[#%module-begin] is the standard one provided
by @racketmodname[racket]. As described in previous
sections, @racket[#%video-module-begin] lifts the defines to
the top of the module, and collects every expression into
one data-structure called @racket[video-begin]. This data
structure compiles into a playlist that is bound to
@racket[vid], and provides that playlist from the module.
The compilation for @racket[#%plain-lambda] also follows
this pattern.

@section[#:tag "impl-ffi"]{Video - Behind the Scenes}

Rather than rendering files directly, Video employs a C
library---The MLT Multimedia Framework@note{libmlt-url}---to
do the actual rendering.MLT Multimedia Framework. This
introduces a common design patterns for embedded DSLs.
First, authors create or find an existing API for a task.
Next, if this API is not implemented in Racket, they create
Racket bindings for this API. As they create these bindings,
they must ensure that the invariants assumed by the Racket
environment are not violated. For example, if a function
will segfault when given bad data, the author must ensure
that this data is never given to the foreign function.
Finally, authors add cleaner front-end primitives and a
surface syntax for their language. Out of the box, the
Racket ecosystem makes this final task trivial. This leaves
would-be language authors with the task of creating bindings
for their favorite API.

As it turns out, the Racket design pattern actually makes
the step of adding bindings trivial. That is to say, when
the task of importing bindings manually is daunting, simply
make a DSL to do it for you. For example, Video employs a
one-off DSL just for building its internal data structures.
The implementation of this DSL is so small, but reduces so
much code, that the entire implementation of the DSL in
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
