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
work than that. Specifically it must define the new
primitive with a different name in terms of the old language
it must then rename the primitive on export to what source
programs in that language expect.

Imagine writing a language where @racket[+] appends two
strings together as well as adding numbers, based on the
kind of value passed to it. The @racket[+] provided by
@racketmodname[racket/base] provides the function for adding
numbers, while @racket[string-append] provides the function
for strings. The language designer simply needs to define a
function, @racket[string+], checks the type of its
arguments, and dispatches to the correct function. From
there, that developer needs to rename @racket[string+] to
@racket[+] when providing functions. The resulting language
implementation fits into 7 lines of code:

@racketmod[
 racket/base
 (provide (except-out racket/base +)
          (rename-out [string+ +]))
 (define (string+ a b)
   (cond [(and (string? a) (string? b)) (string-append a b)]
         [(and (number? a) (number? b)) (+ a b)]
         [else                          (error #,elided)]))]

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

The lifting and combination that Video performs makes use of
a common IR used by all Racket languages. The actual
transformation used for compiling module level code is shown
in @figure-ref["video-begin"], and is written using Racket's
macro system@cite[macros-icfp]. First, the algorithm grabs
the first expression and expands it to a common language
(line 5). Next, the transformation sees if the expanded code
is a list (lines 10 and 16), and if the first element of
that list is one of several recognized symbols (lines 11-14)
e.g. @racket[define] or @racket[provide]. If the first
identifier is recognized, then the macro lifts it out of the
@racket[video-begin], and recursively expands itself without
the newly lifted expression (line 15). On the other hand, if
the expanded is not one of the recognized forms, it is an
expression and gets collected into the @racket[exprs]
collection (line 17). Finally, once @racket[video-begin] has
traversed every piece of syntax (line 3), the @racket[exprs]
list contains all of the module's expressions in reverse
order. It simply defines the given @racket[id] (which is
generally @racket[vid]) to the expressions as a playlist,
and provides the playlist (lines 4-6).

@figure["video-begin" "Video Compilation"]{
@RACKETBLOCK[
(UNSYNTAX @exact{1}) (define-syntax (video-begin stx)
(UNSYNTAX @exact{2})   (syntax-parse stx
(UNSYNTAX @exact{3})     [(_ id exprs)
(UNSYNTAX @exact{4})      #`(begin
(UNSYNTAX @exact{5})          (define id (playlist . #,(reverse (syntax->list #'exprs))))
(UNSYNTAX @exact{6})          (provide id))]
(UNSYNTAX @exact{7})     [(_ id exprs b1 . body)
(UNSYNTAX @exact{8})      (define expanded (local-expand #'b1 'module (UNSYNTAX elided)))
(UNSYNTAX @exact{9})      (syntax-parse expanded
(UNSYNTAX @exact{10})       [(id* . rest)
(UNSYNTAX @exact{11})        #:when (and (identifier? #'id*)
(UNSYNTAX @exact{12})                    (or (free-identifier=? #'id* provide)
(UNSYNTAX @exact{13})                        (free-identifier=? #'id* define)
(UNSYNTAX @exact{14})                        (UNSYNTAX elided)))
(UNSYNTAX @exact{15})        #`(begin #,expanded (video-begin id post-process exprs . body))]
(UNSYNTAX @exact{16})       [_
(UNSYNTAX @exact{17})        #`(video-begin id (#,expanded . exprs) . body)])]))]}

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

The @racket[define-mlt] form is useful for hardening foreign
functions. Racket programmers assume that an error occurs
when a program is used incorrectly, such as a divide by zero
error. Unfortunately, languages such as C do not make this
guarantee. By using @racket[define-mlt], programmers must
only specify a contract@cite[contracts-icfp] that properly
wraps the input and outputs for the function. For example,
consider a function that initializes a C library, @tt{
 mlt_profile_init}, that takes a string and returns either a
profile object, or @tt{NULL} if there is an error. Rather
than having to manually check the input and output types,
the FFI simply just states input and output types, and
errors if a bad input or output type passes through:

@racketblock[
 (define-mlt mlt-profile-init (_fun _string
                                    -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))]

@(compound-paragraph
  (style #f '())
  (list
   @para{The more complex form, @racket[define-constructor],
 interplays with the renderer to convert Video level objects
 into structures that MLT can understand. It also defines the
 fields associated with the object, what their default values
 should be, and what struct it should inherit other
 properties from. For example, the following is the
 description of a Video level producer:}

   (nested
   (minipage
    @racketblock[
 (define-constructor producer service ([type #f] [source #f] [start -1] [end -1])
   (define producer* (mlt-factory-producer (current-profile) type source))
   (mlt-producer-set-in-and-out producer* start end)
   (register-mlt-close mlt-producer-close producer*))])
   
   @exact{\vspace{0.2cm}})
  
   @para{The first form (after @racket[define-constructor])
 gives the name of the struct, in this case
 @racket[producer]. Next, the parent struct
 (@racket[service]) is given. After that, all of the fields
 relevant to producers and their default values are listed.
 Finally, the code to convert this structure into one that
 MLT understands is given. All of the fields listed earlier,
 as well as the parent's fields are available in this
 conversion code. Finally, the renderer calls this conversion
 code at the point when it needs to operate over MLT objects
 rather than Video ones.}))
