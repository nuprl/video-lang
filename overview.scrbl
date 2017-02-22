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

The preceding literature survey suggests that non-linear
video editing distinctly separates the description of a
video clip from the rendering action on it. Specifically, a
video editor needs a description of what the final video
should look like in terms of the given pieces. The action of
creating and rendering this video is a distinct second step.
Going from this assessment to a language design requires one
more idea: abstraction. For example, a description of a
video composition should be able to use a sequence
comprehension to apply a watermark to all images. Similarly,
a professional may wish to create one module per ICFP
presentation in order to make up a complete ICFP channel.
And of course, the language must allow the definition and
use of functions because it is the most common form of
abstraction.

The Video language gets to the heart of the domain. Each Video program
is a complete module that intermingles descriptions of video clips and
auxiliary definitions. It denotes a Racket module that exports a single item: a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. Another way is to import it into a second
Video module and to incorporate it into a larger one. 

@figure["video-example" "A Conference Talk Video"]{
 @racketmod[
 video
 (make-conference-talk video slides audio 125)
 
 (require "conference-lib.vid")
 (define slides (clip "slides05.MTS" #:start 2900 #:end 80000))
 (define video  (playlist (clip "vid01.mp4") (clip "vid02.mp4")
                          #:start 3900 #:end 36850))
 (define audio  (playlist (clip "capture01.wav") (clip "capture02.wav")))]
 @exact{\vspace{0.3cm}} 
  @(centered
   rcon-timeline)}

@Figure-ref["video-example"] shows a talk recording made
with Video. The total running time for the talk is a little
over 22 minutes. A library provides the
@racket[make-conference-video] function which composites
every videos for the conference.

As mentioned, the first line of the program specifies that
this module is written in the Video language. Next, the
second line is the video this module describes. The third
line imports the library that defines the
@racket[make-conference-video] function. Finally, the
remainder is a sequence of definitions used in the second
line. These definitions introduce auxiliary functions and
constants, and can be placed at whatever positioning makes
the program most readable.

Showing the constructing of @racket[make-conference-talk]
demonstrates Videos primitives and combinators, as well as
how developers are productive with it.
First, we
provide Video's primary linguistic mechanisms, binding and
functions. We show this through the definition of the
@racket[make-confernce-video] function. Next, we describe
basic producers: images, clips, colors and so on. Then, we
discuss the basics of how to combine these producers into
playlists and multitracks. To make compelling examples, we
simultaneously introduce transitions, filters, and
properties. Finally, we describe the interface authors use
to render their programs into traditional video files.

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
this to build up large definitions from the inside out. The
rest of the syntax in this figure are video-specific. We
show how these forms are used to create videos.

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
Video programs. A producer is, essentially, a data structure that can be coerced
into video: audio clips, video clips, pictures, and so on.
Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers still.

The simplest type of producer, clips, incorporate traditional
video files. The clip producer converts the file into a sequence
of frames. Developers use clips to import recordings, such as
a conference talk, into their files:

@(split-minipage
  @racketblock[(clip "talk00.mp4")]
  (centered
   (make-playlist-timeline
    #:end #t
    (t# 1)
    (t# 2)
    (t# 3)
    (ellipses)
    (t# 4)
    (t# 5))))

Unlike clips, images are producers of an infinite stream of
frames. Video's combination forms truncate these streams to
fit the length other producers expect. Additionally,
developers can use the @racket[#:length] keyword when they
want a specific number of frames, such as creating an intro
sequence to a conference:

@(split-minipage
  @racketblock[(image "splash.png" #:length 3)]
  (centered (apply make-playlist-timeline #:end #t
                   (make-list 3 (clip-frame (bitmap "res/rcon.png"))))))

@section[#:tag "impl-trans"]{Playlists}

A video is merely a composition of many producers. Video
provides two main ways for combining them: 
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler of the two compositing form.
They are syntactically similar to Racket lists. Any producer
can be put in a playlist including another playlist. Each
clip in the playlist plays in succession. Frequently, video
cameras split recordings into multiple files. With
playlists, developers can easily stitch these files together
to form a single producer:

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

Developers cut playlist, and in general producer, lengths with the
@racket[#:start] and @racket[#:end] keywords. This capacity is
included in video editing because video recordings
frequently start before a talk begins, and ends after the
talk finishes.

@(split-minipage
  @racketblock[(image "logo.jpg" #:length 100)
               talk
               (image "logo.jpg" #:length 100)
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

This example also introduces @racket[define]. Unlike
Racket's definitions or Haskell's @tt{where}-bound ones,
variables defined this way are available in the entire
surrounding module or function scope. Thus, the example
would yield the same result if the definition were at the
top of the code sequence.

@section{Transitions} Jumping from one producer in a
playlist to another can be jarring. Movies frequently reduce
this effect with @emph{transitions}: fading, swiping, etc.
These transitions merge the two adjacent clips in a playlist and
are placed directly inside of possibly implicit playlists. Using the
above example, fade transitions are used to smooth the
transition from logo to video:

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

Unlike playlists, multitracks play producers in parallel. As
with playlists however, they employ transitions to composite
their producers.

Syntactically, multiracks are similar to playlists. The
@racket[multitrack] form accepts a list of producers and
creates a new multitrack producer. Again, transitions are
syntactically placed in the list to combine tracks:

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
specify the coordinates of the top-left corner of the
producer and the the screen space the top producer takes.
Here, the producer following the transition appears in the
top-left hand of the screen and takes up one quarter of the width
and height of the screen.

Transitions within multitracks are not associative; instead,
multitracks interpret transitions in left to right order.
Videos that require a different evaluation order can embed a
multitrack inside of a multitrack, because multitracks are
themselves producers. Using multiple transitions allow
producers to appear side by side rather than just on top of
each other. Modifying the running example from
@secref["impl-trans"] shows a conference video where the
recording of the presenter goes in the top left while the
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

This example modifies the previous example by adding a
second @racket[composite-transition] and placing the the
slides and the recording over a single blank producer. Blank
producers are simply empty producers that act as either a
background for a multitrack, or a filler for a playlist. In
this case, the blank producer is providing a background that
the slides and camera feeds appear on.

@section{Filters}

Filters are similar to transitions, but they modify the
behavior of only a single producer. For example, filters can
remove the color from a clip or change a producer's aspect
ratio. Thus, filters are functions from producers
to producers.

Conference recordings frequently capture audio and video on
separate tracks. Before splicing the tracks together, a
developer may add an envelope filter to provide a fading in
and out effect. Filters can be both applied directly as
functions and attached to producers with the either the
@racket[#:filters] keyword or the @racket[attach-filter]
function. Here is an example of a filer being attached to an
audio track that is itself composited with
the video of the talk above, @racket[compositied-talk]:

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

Producers use two types of properties to store information:
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
   @para{Explicit properties provide a protocol fo
 communicating information from one clip to another. Implicit
 properties exist for the same purpose except that they store
 information that is implicitly associated with a producer,
 such as its length. For example, a conference video may have
 to come with a watermark that is the same length as the
 caputured conference talk. Here is a function that performs this
 operation:}
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

Every Video module is also usable inside of other Video
programs and general-purpose Racket programs. Each module
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

Unlike Racket programs, Video programs use
@racket[include-video] to reduce naming conflicts. That is,
rather than requiring several videos at the module level,
renaming the newly imported @racket[vid] identify to
something unique, and placing that identifier in the
appropriate place, developers simply insert
@racket[include-video] at the point where they want the
imported video to go.
