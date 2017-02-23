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

Using the Racket ecosystem allows developers to
implement languages quickly and easily. Furthermore, these languages
compose so that modules written in one language can easily
interact with modules in another. Best of all, the
implementation of one language may take advantage of other languages.
The upshot here is that implementing
Video is as simple as implementing video-specific language
constructs, while leaving the general-purpose bits to Racket.

This section discusses how a developer can implement a DSL
in Racket (@secref["impl-create"]). Video serves as the
running example of a DSL that follows this design
(@secref["impl-video"]). Finally, not only is Video a Racket
DSL, but part of the implementation of Video is implemented
using the Racket doctrine. That is, Video is implemented in
a DSL designed specifically for implementing Video, a
language that itself is implemented in Racket
(@secref["impl-ffi"]).

@section[#:tag "impl-create"]{Creating Languages, the Racket Way}

Creating Racket DSLs is straightforward. Language authors simply
remove unwanted features from some base language, add new
ones, and alter existing features to match the
semantics of the new domain.

Adding and removing features is simple because a language implementation is
a module like any other module in Racket. Language authors
create a module that defines the new features, export those
features, and does not export unwanted ones. They do this in
the same manner as a programmer augmenting the functionality
of a library via a wrapper module@cite[lal-pldi]. Unlike adding or removing
features, modifying existing features requires a little more
work than that. Specifically it must define the new
primitive with a different name in terms of the old
language. Additionally, the language must rename that primitive on
export to what source programs in that language expect.

Imagine writing a language where variable reassignment
(using @racket[set!] syntax) logs a warning of its use. The
@racket[set!] provided by @racketmodname[racket/base]
provides the functionality for reassignment, while
@racket[log-warning] provides the logging functionality. The
language designer simply needs to define a new syntax,
@racket[warning:set!], that logs its use and performs the
assignment. From there, that developer needs to rename
@racket[warning:set!] to @racket[set!] when exporting the
language's primitives. This renaming makes the new
functionality available to anyone expecting @racket[set!] in
the new language. Here is the resulting implementation:

@racketmod[
 racket/base
 (provide (rename-out warning:set! set!)
          (except-out racket/base set!))
 (define-syntax (warning:set! stx)
   (syntax-parse stx
     [(_ id val)
      #'(begin
          (log-warning "Warning, reassigning ~a" id)
          (set! id val))]))]

In addition to syntactic extensibility, Racket's syntax
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
protocol works because the Racket compiler places it in
front of every function application. Thus, language authors
only need to implement their version of @racket[#%app] in
terms of an existing one. Using the pattern from above, lazy
application is implemented as:

@(nested (minipage @racketmod[
 racket/base
 (provide (rename-out #%lazy-app #%app)
          (excpet-out racket/base #%app))

 (define-syntax (#%lazy-app stx)
   (syntax-parse stx
     [(_ rater rand ...)
      #'(_ (force rater) (lazy rand)  ...)]))]))

@exact{\vspace{0.2cm}}

When a programmer uses this new language, Racket inserts the
language's @racket[#%app] at call sites, in this case,
@racket[#%app]@superscript{lazy}. From there, Racket
resolves the renaming and expands @racket[#%lazy-app] into
@racket[#%app]@superscript{internal}. The resulting
expansion looks like:

@(nested (minipage @racketblock[
 (f a b c ...) @#,->text{compiles to} (@#,elem{@racket[#%app]@superscript{lazy}} f a b c ...)
               @#,->text{compiles to} (#%lazy-app f a b c ...)
               @#,->text{compiles to} (@#,elem{@racket[#%app]@superscript{internal}} (force f) (lazy a) (lazy b) (lazy c) ...)]))

@section[#:tag "impl-video"]{The Essence of Video}

Video's implementation is spread across several main
components: a surface syntax, a core library, and a
rendering layer. It consists of approximately 3,200 lines of
code. Of that, about 95 lines are specific to the surface
syntax, and 353 lines define the video-specific primitives
the language uses.@note{The remaining lines are for GUI
 tools, the FFI, and the renderer.} The latter are
implemented using standard functional programming
techniques. They only serve as a thin wrapper of functions
to Video's core data-types.

@; #%plain-lambda -> #%lambda-begin?

Surface syntax is where Video uses two of Racket's
interposition points, namely @racket[#%module-begin] and
@racket[#%plain-lambda]. These points enable developers to
define the semantics of the module-level and local function-level
scopes respectively. As with @racket[#%app], using these
forms enables language authors to reuse the syntax of an existing
language, while still being able to reinterpret both whole modules
and local pieces.

Similarly to @racket[#%app], @racket[#%module-begin] gets
placed at the start of every module. Just as replacing
@racket[#%app] enables languages authors to reinterpret
function application, replacing @racket[#%module-begin]
allows languages to make context sensitive transofmrations.
Thus, Video can lift definitions and collecting expressions
by creating its own @racket[#%module-begin] form.

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
@racket[#%module-begin] for Video, written using Racket's
@racket[syntax-parse] system@cite[fortifying-jfp]. As before, the syntax is
defined with a different name, @racket[#%video-module-begin]
(line 6), and is renamed on export (line 3). The
implementation of @racket[#%video-module-begin] expands to
@racket[video-begin], passing the @racket[vid] identifier
(line 9). The @racket[video-begin] syntax (lines 11-25) is
responsible for lifting definitions and accumulating
expressions into a playlist.

The definition lifting @racket[video-begin] performs makes
use of a common core language used by all Racket languages.
First, the algorithm grabs the first expression and expands
it to a common language (line 18). Next, the transformation
sees if the expanded code is a list (lines 20 and 24), and
if the first element of that list is one of several
recognized symbols (lines 21-22) e.g. @racket[define] or
@racket[provide]. If the first identifier is recognized,
then the macro lifts it out of the @racket[video-begin], and
recursively expands itself without the newly lifted
expression (line 23). On the other hand, if the expanded is
not one of the recognized forms, it is an expression and
gets collected into the @racket[exprs] collection (line 25).
Finally, once @racket[video-begin] has traversed every piece
of syntax (line 13), the @racket[exprs] list contains all of
the module's expressions in reverse order. It simply defines
the given @racket[id] (which is generally @racket[vid]) to
the expressions as a playlist, and provides the playlist
(lines 14-16).

It is worth noting that @racket[video-begin] for function
scope differs only in the base case. Rather than defining
and providing an identifier for the resulting playlist, it
must only return the constructed playlist. The recursive
code (lines 7 through 17) remain the same for both function
contexts and module contexts.

@Figure-ref["mod-begin"] shows the compilation of a module
for a conference talk. Here,
@racket[#%module-begin]@superscript{video} is Video's
variant, while @racket[#%module-begin]@superscript{internal}
is the standard one provided by @racketmodname[racket]. Like
earlier, @racket[#%module-begin]@superscript{video} gets
resolved to @racket[#%video-module-begin]. Also as described
informally in preceding sections, the purpose of
@racket[#%video-module-begin] is to lift the definitions to
the top of the module and collect the remaining expressions
into one data-structure. This data structure is a playlist
bound to @racket[vid]. Additionally, it gets exported from
the module. The @racket[#%plain-lambda] The
@racket[#%plain-lambda] follows this same pattern, with the
exception of returning @racket[vid] rather than exporting it.

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
  @racketmod0[
 video
 (@#,elem{@racket[#%module-begin]@superscript{video}}
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
  @racketmod0[
 racket/base
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
  @racketmod0[
 racket/base
 (@#,elem{@racket[#%module-begin]@superscript{internal}}
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
  @racketmod0[
 racket/base
 (@#,elem{@racket[#%module-begin]@superscript{internal}}
  (define video #,elided)
  (provide vid)
  (define vid
    (playlist
     (image "splash.png" #,elided)
     (make-conf-video video #,elided))))])
 @exact{\vspace{0.2cm}}}


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
or out of bounds lookup. Unfortunately, languages such as C
do not make this guarantee. By using @racket[define-mlt],
programmers must only specify a
contract@cite[contracts-icfp] that properly wraps the inputs
and outputs for the function. For example, consider a
function that initializes a C library, @tt{
 mlt_profile_init}, that takes a string and returns either a
profile object, or @tt{NULL} if there is an error. Rather
than having to manually check the input and output values,
the FFI simply just states input and output types, and
errors if a bad input or output type passes through:

@(nested (minipage @racketblock[
 (define-mlt mlt-profile-init (_fun _string
                                    -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))]))

@exact{\vspace{0.2cm}}

@(compound-paragraph
  (style #f '())
  (list
   @para{The more complex form, @racket[define-constructor],
 interplays with the renderer to convert Video level objects
 into structures that MLT can understand. It also defines the
 fields associated with the object, what their default values
 should be, and what struct it should inherit other
 fields from. For example, the following is the
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
