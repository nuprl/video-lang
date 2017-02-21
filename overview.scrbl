#lang scribble/sigplan

@require[scriblib/figure
         (except-in scribble/core table paragraph)
         (except-in scribble/manual cite)
         scribble/example
         scriblib/footnote
         pict
         (prefix-in pict: pict/code)
         (except-in pict/code typeset-code code)
         racket/format
         racket/list
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)
@(get-current-code-font-size (λ () font-size))
@(current-code-line-sep code-line-sep)
@(define blank-clip (clip-scale (blank 1)))

@title[#:tag "overview"]{The Producers}

The preceding literature survey suggests that non-linear video
editing distinctly separates the description of a video clip
from the rendering action on it. Specifically, a video
editor needs a description of what the final video should
look like in terms of the given pieces. The action of
creating and rendering this video is a distinct second step.
Going from this assessment to a language design, requires one more idea: 
abstraction. for example, a declarative-functional description of a video
composition should be able to use a
comprehension to apply a watermark to all images. Or, a professional may
wish to create one module per ICFP presentation in order to make up a
complete ICFP channel.

The Video language gets to the heart of the problem. Each Video program
is a complete module that intermingles descriptions of video clips and
functions. It denotes a module that exports a single item: a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. Another way is to import it into another
Video module and to incorporate the exported video clip description into a
larger one. 

@figure["video-example" "A Conference Talk Video"]{
 @racketmod[
 video
 (make-conference-talk video slides audio 125)
 
 (require "conference-lib.vid")
 (define slides (clip "0005.MTS" #:start 2900 #:end 80000))
 (define video (playlist (clip "0001.mp4") (clip "0002.mp4")
                         #:start 3900 #:end 36850))
 (define audio (playlist (clip "0001.wav") (clip "0002.wav")))]
 @exact{\vspace{0.3cm}} 
  @(centered
   rcon-timeline)}

@Figure-ref["video-example"] shows a talk recording made
with Video. The total running time for the talk is a little
over 22 minutes, and the program is only 7 lines long. A
library does provide the @racket[make-conference-video]
function, but this function composites every conference viis used by all videos for the
conference.

Like Unix shell scripts, the first line of the program
specifies that this module is written in the Video language.
Next, the second line is the video this module describes. The
third line imports the library that defines the @racket[make-conference-video] function.
Finally, the remaining program is an
sequence of definitions used in the second line.
These definitions introduce auxiliary functions and constants, and
can be placed at whatever positioning makes the program most
readable.

Showing the constructing of @racket[make-conference-talk]
demonstrates Videos primitives and combinators, as well as
how developers are productive with it. Readers not
interested in Video's design, but want to see an analysis of
its use can skip to @secref["case-study"]. That section
shows how much effort goes into making conference
recordings, both in terms of time and code length. First, we
describe basic producers: images, clips, colors and so on.
Then, we discuss the basics of how to combine these
producers into playlists and multitracks. To make compelling
examples, we simultaneously introduce transitions, filters,
and properties. Finally, we describe the interface authors
use to render their programs into traditional video files.

@section[#:tag "overview-functions"]{Functions and binding}

Functions and module scope are similar in Video. They both
lift definitions and describe a video with the remaining
expressions. The main difference is that functions provide a
video, while functions return one.@note{It is also possible
 to write functions for non-videos, we leave the details to
 the documentation.} In effect, Video modules are first order
functions that are capable of separate compilation. For the
reason, the implementation and use of the
@racket[make-conference-talk] function used in
@figure-ref["video-example"] look syntactically similar. The
upshot is that functions serve as a natural way to
compose videos.

The implementation for @racket[make-conference-talk] is
shown in @figure-ref["video-functions"]. Lines 2-17 show
internal definitions local to the function, while line 18 is
the return value for the function. While the returned
expression is at the bottom, it could just as easily appear
at the top on line 2 before the definitions. This figure
also introduces @racket[define*]. Rather than creating a
recursive definition, @racket[define*] replaces any old
binding with that name with a new one. Video editors use
this to build up large definitions from the inside out.

@figure["video-functions" "Some Text"]{
 @racketblock[
@#,exact{1} (define (make-conference-talk video slides audio offset)
@#,exact{2}   (define clean-audio (playlist (blank offset)
@#,exact{3}                                 (attach-filter audio
@#,exact{4}                                                (envelope-filter 50 #:direction 'in)
@#,exact{5}                                                (envelope-filter 50 #:direction 'out))))
@#,exact{6}   (define* _ (multitrack (blank #f)
@#,exact{7}                          (composite-transition 0 0 1/4 1/4)
@#,exact{8}                          slides
@#,exact{9}                          (composite-transition 1/4 0 3/4 1)
@#,exact{10}                         video
@#,exact{11}                         (composite-transition 0 1/4 1/4 3/4)
@#,exact{12}                         (image "logo.png" #:length (producer-length talk))))
@#,exact{13}  (define* _ (playlist (image "splash.png" #:length 100)
@#,exact{14}                          (fade-transition #:length 50)
@#,exact{15}                          _
@#,exact{16}                          (fade-transition #:length 50)
@#,exact{17}                          (image "splash.png" #:length 100)))
@#,exact{18}  (multitrack _ clean-audio))]}

@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for
Video programs. A producer is any data that can be coerced
into video: audio clips, video clips, pictures, and even
some data structures. Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers still.

The simplest type of producer, clips, are simply traditional
video files. The producer converts the file into a sequence
of frames. Creators use clips to import recordings, such as
a conference talk, into their files:

@(split-minipage
  @racketblock[(clip "talk00.MTS")]
  (centered
   (make-playlist-timeline
    #:end #t
    (t# 1)
    (t# 2)
    (t# 3)
    (ellipses)
    (t# 4)
    (t# 5))))

Unlike clips, images are producers with an unspecified
length. Producers with an unspecified length produce an
infinite stream of frames. Video's combination forms
truncate these streams to fit the length other producers
expect. However, authors can use @racket[#:length] when they
want a specific number of frames, such as creating an intro
sequence to a conference:

@(split-minipage
  @racketblock[(image "splash.png" #:length 3)]
  (centered (apply make-playlist-timeline #:end #t
                   (make-list 3 (clip-frame (bitmap "res/rcon.png"))))))

@section{Playlists}

A video is merely a composition of many producers. Video
provides two main ways for combining them: 
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler of the two compositing form.
They are syntactically similar to lists. Any producer can be
put in a playlist including another playlist. Each clip in
the playlist plays in succession. Frequently, video cameras
split recordings in multiple files. Playlists stitch these
files together to form a single producer.

@(split-minipage
  @racketblock[(playlist (clip "talk00.MTS")
                         (clip "talk01.MTS"))]
  (centered (make-playlist-timeline
             #:end #t
             (t# 1)
             (t# 2)
             (ellipses)
             (t# 3)
             (t# 4)
             (t# 5)
             (t# 6)
             (ellipses)
             (t# 7)
             (t# 8))))

Programmers cut playlist and producer lengths with the
@racket[#:start] and @racket[#:end] keywords. This step is
important in video editin because video recordings
frequently start before a talk begins, and ends after the
talk finishes.

@(split-minipage
  @racketblock[(image "splash.jpg" #:length 100)
               talk
               (image "splash.jpg" #:length 100)
               (define talk
                 (playlist (clip "talk00.MTS")
                           (clip "talk01.MTS")
                           #:start 100
                           #:end 8000))]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# 1)
             (t# 2)
             (t# 3)
             (ellipses)
             (t# 4)
             (t# 5)
             (t# 6)
             (ellipses))))

This sample also introduces binding using @racket[define].
Unlike with Racket's @racket[define] keyword or Haskell's
@tt{where} keyword, variables bound this way are available
in the entire module or function they are defined in. Thus,
the example would yield the same result if the definition is
at the top of the code.

@section{Transitions} Jumping from one producer in a
playlist to another is jarring. Movies frequently reduce
this effect with @emph{transitions}: fading, swiping, etc.
These transitions merge two adjacent clips in a playlist and
are placed directly inside the playlists syntax. Using the
previous example, fade transitions are used to smooth the
transition from logo to video:@note{Remember that playlists
 are implicit in Video modules. Also, the @racket[talk]
 variable is elided.}

@(split-minipage
    #:split-location 0.45
    @racketblock[(image "splash.png" #:length 100)
                 (fade-transition #:length 50)
                 talk
                 (fade-transition #:length 50)
                 (image "splash.jpg" #:length 100)]
      (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# 1)
             (t# 2)
             (t# 3)
             (ellipses)
             (t# 4)
             (t# 5)
             (t# 6)
             (ellipses))))

Every transition in a playlist actually shortens the length
of the playlist, because transitions produce one clip for
every two clips they consume. Additionally, playlists may
contain multiple transitions. Videos that contain this
behavior, like above, are not ambiguous because playlist transitions are
associative operations. Thus, multiple transitions placed in a
single playlist described the desired clip without any
surprises.

@section{Multitracks}

Unlike playlists, multitracks play producers in parallel.
Though, as with playlists, they employ transition to
composite their producers.

Syntactically, multiracks are similar to playlists. The
@racket[multitrack] form accepts a list of producers and
creates a new multitrack producer. Again, transitions are
syntactically placed in the list to combine tracks.

@(split-minipage
  @racketblock[(multitrack
                (clip "slides.mp4")
                (composite-transition 0 0 1/4 1/4)
                talk)]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# 1)
             (t# 2)
             (t# 3)
             (t# 4)
             (ellipses))))

This example uses @racket[composite-transition], which
places one producer on top of the other. The four constants
used indicate the coordinates to place the top-left corner
of the producer, and the other two indicate the screen space
the top producer takes. Here, the top producer appears in
the top-left hand of the screen, and takes up half of the
width and height of the screen.

Transitions within multitracks are not associative; instead,
multricks interpret transitions in left to right order.
Videos that require a different evaluation order can embed a
multitrack inside of a multitrack, because multitracks are
themselves producers. This technique allows producers to
appear side by side rather than just on top of each other.
Modifying the example above shows a conference video where
the recording of the present goes in the top left, while the
slides go on the right:

@(split-minipage
  @racketblock[(multitrack
                (blank #f)
                (composite-transition 0 0 1/4 1/4)
                (clip "slides.mp4")
                (composite-transition 1/4 0 3/4 1)
                talk)]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# 1)
             (t# 2)
             (t# 3)
             (t# 4)
             (ellipses))))
  
@; Keep?
@;{
An alternative is to apply multiple transitions to the same
track. To support this alternative, multitracks support the
@racket[#:transitions] keyword, which takes a list of
transitions. Each transition in this list composites a pair
of producers, top and bottom, in the multitrack. These
producers are specified with the @racket[#:top] and
@racket[#:bottom] keywords respectively, and must be
producers found in the multitrack.

The @racket[#:transitions] keyword also applies to
playlists.
This allows abstracting over both playlist and multitrack
functions that insert transitions into both of these data
representations without having to bother with list
operations directly. Here is an example:}

@section{Filters}

Filters are similar to transitions, but they modify the
behavior of only a single producer. For example, filters can
remove the color from a clip or change a producer's aspect
ratio. Thus, filters themselves are functions from producers
to producers.

Conference recordings frequently capture audio and video on
separate tracks. Before splicing the tracks together,
frequently add an envelope filter to provide a fading in and
out feel. Filters can be applied directly as functions, or
attached to producers with the @racket[#:filters] keyword.
This is an example a filer being attached an audio track
that is itself composited with the video of the talk above:

@(split-minipage
  #:split-location 0.58
  @racketblock[(multitrack
                composed-talk
                (clip "0000.wav"
                      #:filters (list
                                 (envelope-filter 50 #:direction 'in)
                                 (envelope-filter 50 #:direction 'out))))]
  (vc-append
   25
   (hc-append 20
              (scale-to-fit (bitmap "res/sound-start.png") 90 30 #:mode 'distort)
              (ellipses)
              (scale-to-fit (bitmap "res/sound-end.png") 90 30 #:mode 'distort))
   (make-playlist-timeline
    #:end #t
    (clip-scale (bitmap "res/rcon.png"))
    (ellipses)
    (clip-scale (bitmap "res/stephen.jpg"))
    (ellipses)
    (clip-scale (bitmap "res/rcon.png")))))


@section{Properties and Dependent Clips}

Producers use properties to store and retrieve information
about other producers. Properties come in two varieties:
implicit and explicit properties. Implicit properties are
innate to clips, for example, length and dimensions.
Explicit properties must be added by the program itself.

@(compound-paragraph
  (style #f '())
  (list
   @para{The properties API has two calls:}
   @itemlist[
 @item{@racket[(set-property #,(emph "producer") #,(emph "key") #,(emph "value"))] creates
  an explicit property. It returns a new producer with @emph{key} associated with @emph{value}.}
 @item{@racket[(get-property #,(emph "producer") #,(emph "key"))] returns
  the value associated with @emph{key}. If the property is set
  both implicitly and explicitly, the explicit property has priority.}]
   @para{Explicit properties provide a protocol to
 communicate information from one clip to another. Implicit
 properties exist for the same purpose except that they store
 information that are implicitly associated with a producer,
 such as its length.
 For example, the conference video can also have a watermark that is
 the same length as the conference video.}
   (split-minipage
    @racketblock[(multitrack
                  (blank #f)
                  (composite-transition 0 0 1/4 1/4)
                  (clip "slides.mp4")
                  (composite-transition 1/4 0 3/4 1)
                  talk
                  (composite-transition 0 1/4 1/4 3/4)
                  (image "logo.png" #:length (producer-length talk)))]
    (centered (make-playlist-timeline
               #:end #t
               (ellipses)
               (t# 1)
               (t# 2)
               (t# 3)
               (t# 4)
               (ellipses))))))

@section[#:tag "overview-rendering"]{From Programs to Videos}

Video programs may describe standalone modules or pieces of
a larger production. In the first case, users give Video
modules to a renderer. In the second case, other modules
import the target module before rendering.

A renderer converts standalone Video programs to traditional
videos. This renderer allows creators to set various
properties such as aspect ratio, frame rate, and even output
format. For example, the @racket[preview-video] function
renders the video on the fly and plays it in a preview
window:

@(split-minipage
  (centered @racket[(preview-video "talk.vid")])
  (centered (scale (bitmap "res/talk-preview.png") 0.08)))

Every Video program is usable inside of other Video
programs and general purpose Racket programs. Each module
in the language exports one @racket[vid] identifier, that
contains the described film.

Modules written in Video export a data structure bound to
@racket[vid]. This data structure can itself be used in
large video projects through @racket[include-video].

The top half of @figure-ref["video-use"] shows the program
for the green video used above. The code to the right shows
the contents of the @racket[vid] structure. This video is
itself a producer and can be used in larger projects, shown
in the bottom half of @figure-ref["video-use"]. Video
provides @racket[include-video] to import video files into
larger contexts. This function imports the specified file
and places the @racket[vid] struct where it is placed. For
example, if a broadcaster for an already edited conference
video wants to add an additional splash to the front, they
could easily do so:

@(split-minipage
  @racketblock[(image "broadcast.png" #:length 100)
               (include-video "talk.vid")]
  (centered (make-playlist-timeline
             #:end #t
             (t# 1)
             (t# 2)
             (ellipses)
             (t# 3)
             (t# 4)
             (t# 5)
             (ellipses))))
