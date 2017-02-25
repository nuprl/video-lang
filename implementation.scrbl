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

This section discusses how a developer can implement a DSL
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
Language authors create a module that defines the new
features, export those features, and do not export unwanted
ones. They do so in the same manner as a programmer
augmenting the functionality of a library via a wrapper
module. In contrast, modifying existing features requires a
little more work than that. Specifically the module must
define a syntax transformation in terms of the old language
using an arbitrary but distinct name, and rename that
primitive on export.

Imagine writing a language where variable reassignment
(using @racket[set!] syntax) logs a warning of its use. The
@racket[set!] provided by @racketmodname[racket/base]
provides the functionality for reassignment, while
@racket[log-warning] from the same language provides the
logging functionality. The language designer needs to define
a new syntax, let's call it @racket[warning:set!], that logs
its use and performs the assignment. From there, the
developer needs to rename @racket[warning:set!] to
@racket[set!] when exporting the language's primitives. This
renaming makes the new functionality available to anyone
expecting @racket[set!] in the nested language. Here is the
resulting implementation:

@racketmod[
 racket/base
 (provide (rename-out warning:set! set!)
          (except-out (all-from-out racket/base) set!))
 (define-syntax (warning:set! stx)
   (syntax-parse stx
     [(_ id val)
      #'(begin (log-warning "Warning, reassigning ~a" id)
               (set! id val))]))]

In addition to syntax extensions, Racket's syntax
system supports several interposition points that facilitate
language creation: @racket[#%app] for function application,
@racket[#%module-begin] for module boundaries, @racket[#%datum] for
literals, @racket[#%provide] for module exports, and so on.
The purpose of these points is to allow language authors to
alter the semantics of the relevant features without changing the surface syntax.
As an example, a developer can
use a strict @racket[#%app] to turn a language with
call-by-value semantics into one with call-by-need
semantics. The @racketmodname[lazy/racket] language uses
this interposition point to convert @racketmodname[racket],
a language that employs strict semantics, into an otherwise
equivalent language with lazy semantics. The @racket[#%app]
protocol works because the Racket compiler places the marker in
front of every function application. Thus, language authors
only need to implement their version of @racket[#%app] in
terms of an existing one:

@(nested (minipage @racketmod[
 racket/base
 (provide (rename-out #%lazy-app #%app)
          (excpet-out (all-from-out racket/base) #%app))

 (define-syntax (#%lazy-app stx)
   (syntax-parse stx
     [(_ rater rand ...)
      #'(#%app (force rater) (lazy rand)  ...)]))]))

@exact{\vspace{0.2cm}}

When a programmer uses this new language, Racket inserts the
language's @racket[#%app] at call sites, in this case, a reference to
@racket[#%app]@superscript{lazy}. From there, Racket
resolves the renaming and expands @racket[#%lazy-app] into
@racket[#%app]@superscript{internal}. The resulting
elaboration process looks like:

@(nested (minipage @racketblock[
 (f a b c ...) @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{lazy}} f a b c ...)
               @#,->text{elaborates to} (#%lazy-app f a b c ...)
               @#,->text{elaborates to} (@#,elem{@racket[#%app]@superscript{internal}} (force f) (lazy a) (lazy b) (lazy c) ...)]))

@section[#:tag "impl-video"]{The Essence of Video}

Video's implementation is spread across several main
components: a surface syntax, a core library, and a
rendering layer. It consists of approximately 2,400 lines of
code. Of that, about 90 lines are specific to the surface
syntax, and 350 lines define the video-specific primitives
the language uses. The latter are implemented using standard
functional programming techniques. They only serve as a thin
wrapper of functions to Video's core data-types. The
remaining lines are for the FFI and renderer.

Surface syntax is where Video uses two of Racket's
interposition points, namely @racket[#%module-begin] and
@racket[#%plain-lambda]. These points enable developers to
define the semantics of the module-level and local function-level
scopes respectively. As with @racket[#%app], using these
forms enables language authors to reuse the syntax of an existing
language, while still being able to reinterpret both whole modules
and local pieces.

Similar to @racket[#%app], @racket[#%module-begin] gets
placed at the start of every module and wraps the entire
contents of that module. This enables languages authors to
implement context-sensitive transofmrations. It is with
@racket[#%module-begin] that Video can lift definitions and
collect expressions.

@figure["video-begin" "Video Compilation"]{
@racketblock[
#:escape L
(L @exact{1})   (L (hash-lang)) racket/base
(L @exact{2}) 
(L @exact{3})   (provide (rename-out [#%video-module-begin module-begin])
(L @exact{4})            (except-out #%module-begin racket/base))
(L @exact{5})  
(L @exact{6})   (define-syntax (#%video-module-begin stx)
(L @exact{7})     (syntax-parse stx
(L @exact{8})       [(_ . body)
(L @exact{9})        #'(#%module-begin (video-begin vid . body))]))
(L @exact{10})  
(L @exact{11})  (define-syntax (video-begin stx)
(L @exact{12})    (syntax-parse stx
(L @exact{13})      [(_ id exprs)
(L @exact{14})       #`(begin
(L @exact{15})           (define id (playlist . #,(reverse (syntax->list #'exprs))))
(L @exact{16})           (provide id))]
(L @exact{17})      [(_ id exprs b1 . body)
(L @exact{18})       (define expanded (local-expand #'b1 'module (L elided)))
(L @exact{19})       (syntax-parse expanded
(L @exact{20})         [(id* . rest)
(L @exact{21})          #:when (and (identifier? #'id*)
(L @exact{22})                      (matches-one-of? #'id* (list #'provide #'define (L elided))))
(L @exact{23})          #`(begin #,expanded (video-begin id post-process exprs . body))]
(L @exact{24})         [_
(L @exact{25})          #`(video-begin id (#,expanded . exprs) . body)])]))]}

@Figure-ref["video-begin"] shows the implementation of
@racket[#%module-begin] for Video, written with Racket's
@racket[syntax-parse] system@cite[fortifying-jfp]. As
before, the syntax is defined with a different name,
@racket[#%video-module-begin] (line 6), and is renamed on
export (line 3). The implementation of
@racket[#%video-module-begin] expands to
@racket[video-begin], passing the @racket[vid] identifier
(line 9). The @racket[video-begin] syntax transformation
(lines 11-25) is responsible for lifting definitions and
accumulating expressions into a playlist.

As with @racket[#%video-module-begin], the definition for
@racket[video-begin] uses pattern matching. Lines 13 and 17
specify the two pattern cases for the transformation, one for
when the module is empty and another one that handles
the non-empty sequence. In this second case, the transformer grabs the first
expression and expands it to a common language (line 18).
This common language is simply elaborated Racket code.
Next, the transformation checks whether the elaborated code is a list
(lines 20 and 24), and if the first element of that list is
one of several recognized identifiers (lines 21-22), e.g.,
@racket[define] or @racket[provide]. If the first identifier
is recognized, then the transformer lifts it out of the
@racket[video-begin] and recursively calls itself without
the newly lifted expression (line 23). Otherwise, it is an
expression and gets collected into the @racket[exprs]
collection (line 25). Finally, once @racket[video-begin] has
traversed every piece of syntax (line 13), the
@racket[exprs] list contains all of the module's expressions
in reverse order. The code generated in response to this
pattern defines the given @racket[id] (which is generally
@racket[vid]) to the expressions bundled as a playlist and
provides the playlist (lines 14-16).

The @racket[video-begin] transformer also works for function
scope. It differs only in the base case. Rather than
defining and providing an identifier for the resulting
playlist, it mainly returns the constructed playlist. The
recursive call (lines 7 through 17) remains the same.
Naturally, the definitions can be abstracted still.

@Figure-ref["mod-begin"] shows the syntax elaboration of a
module using the Video specific @racket[#%module-begin]
form. The elaborated module describes the running conference
talk example. Here, @racket[#%module-begin]@superscript{
 video} is Video's variant, while
@racket[#%module-begin]@superscript{internal} is the one
provided by @racketmodname[racket].

@figure["mod-begin" "Compilation for a Video Module"]{
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
 (@#,elem{@racket[#%module-begin]@superscript{internal}}
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
 (@#,elem{@racket[#%module-begin]@superscript{internal}}
  (define video #,elided)
  (provide vid)
  (define vid
    (playlist
     (image "splash.png" #,elided)
     (conference-video video #,elided))))])
 @exact{\vspace{0.2cm}}}


@section[#:tag "impl-ffi"]{Video, Behind the Scenes}

Video relies on bindings to a C library---the MLT Multimedia
Framework@note[mlt-url]---to perform the rendering of video
descriptions to files or streams.

As it turns out, the Racket doctrine actually applies to
the step of adding bindings too. That is to say, because
the task of importing bindings manually is daunting, we created
a DSL for just this task instead.
The implementation of this DSL is small but eliminates so
much boilerplate code  that the entire implementation of the DSL in
combination with the actual back-end of Video is smaller
than an otherwise identical back-end that does not use a
DSL. In short, creating the DSL sufficiently reduces the overall effort that it
offsets the startup cost, even though it is used @bold{only once}.

The auxiliary DSL injects two forms into Racket:
@racket[define-mlt] and @racket[define-constructor]. The
first form introduces a small DSL that combines with the
Racket FFI to facilitate the import of bindings from MLT.
The second form, @racket[define-constructor], defines the
core data types for Video. Additionally, this form
cooperates with the renderer to convert these data types
into data that MLT understands.

@(compound-paragraph
  (style #f '())
  (list
   @para{The @racket[define-mlt] form is useful for hardening foreign
functions. By using @racket[define-mlt],
programmers must only specify a
contract@cite[contracts-icfp] that describes the valid inputs
and outputs. Consider @tt{mlt_profile_init}, a
function that initializes a C library.
It takes a string and returns either a
profile object or @tt{NULL} if there is an error. Rather
than having to manually check the input and output values,
the FFI just states input and output types:}

@(nested (minipage @racketblock[
 (define-mlt mlt-profile-init (_fun _string
                                    -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))]))

@para{
@exact{\vspace{0.2cm}}
It errors if a bad input or output type passes through this interface.}))

@(compound-paragraph
  (style #f '())
  (list
   @para{The @racket[define-constructor] form
 interplays with the renderer to convert Video-level objects
 into structures that MLT can understand. It also defines the
 fields associated with the object, what their default values
 should be, and what struct it should inherit other
 fields from. For example, the following is the
 description of a Video-level producer:}

   (nested
   (minipage
    @racketblock[
 (define-constructor producer service ([type #f] [source #f] [start -1] [end -1])
   (define producer* (mlt-factory-producer (current-profile) type source))
   (mlt-producer-set-in-and-out producer* start end)
   (register-mlt-close mlt-producer-close producer*))])
   
   @exact{\vspace{0.2cm}})
  
   @para{The first identiier is
 the name of the struct, in this case
 @racket[producer]. After that, the parent struct
 (@racket[service]) is the parent struct. Next, all of the fields
 relevant to producers and their default values are listed.
 Finally, the code to convert this structure into one that
 MLT understands is given. All of the fields listed earlier,
 as well as the parent's fields are available in this
 conversion code. The renderer calls this conversion
 code at the point when it needs to operate over MLT objects
 rather than Video ones.}))

@TODO{The above paragraph is trying to say that structs
 created with @racket[define-constructor] have primarily
 three additions over regular structs. First, they inherit
 some additional properties of object-oriented behavior above
 and beyond what structs give. (Namely, dynamic dispatch and
 copy constructors). Second, nicer construtors that can take
 advantage of the parent fields without needing to specify
 them every time the constructor is called. Third, a body
 that converts the struct to a binary format that MLT
 understands. This conversion function has direct access to
 all of the fields defined in the struct, including parent
 fields. I tried to trim this information down to stuff
 pertinent to the paper.}
