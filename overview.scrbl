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

The literature survey suggests that non-linear
video editing distinctly separates the description of a
video clip from the rendering action on it. Specifically, a
video editor needs a description of what the final video
should look like in terms of the given pieces. The action of
rendering this video is a distinct second step.
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
 (require "conference-lib.vid")
 
 (conference-talk video slides audio 125)
 (code:comment @#,elem{where})
 (define slides (clip "slides05.MTS" #:start 2900 #:end 80000))
 (define video  (playlist (clip "vid01.mp4") (clip "vid02.mp4")
                          #:start 3900 #:end 36850))
 (define audio  (playlist (clip "capture01.wav") (clip "capture02.wav")))]
 @exact{\vspace{0.3cm}} 
  @(centered
   rcon-timeline)}

@Figure-ref["video-example"] shows a sample program written
in Video. This program is a description of an edited
conference video. It combines a recording of the speaker, a
capture of the slides, and even begins and ends the video
with the conference logo.

As mentioned, the first line of the program specifies that
this module is written in the Video language. Next, the
second line imports the library that defines the
@racket[conference-talk] function used by the module. The
third line is the actual video this module describes. It
uses @racket[conference-talk] to combine the actual feeds:
@racket[video], @racket[slides] and @racket[audio]. Finally,
the remainder is a sequence of definitions used in the
third line. These definitions introduce auxiliary functions
and constants; they can be placed at whatever positioning
makes the program most readable.

Showing the constructing of @racket[conference-talk]
demonstrates Videos primitives and combinators, as well as
how developers are productive with it.
First, we
provide Video's primary linguistic mechanisms, binding and
functions (@secref["overview-functions"]). We show this through the definition of the
@racket[confernce-video] function. Next, we describe
basic producers (@secref["overview-simple"]): images, clips, colors and so on. Then, we
discuss the basics of how to combine these producers into
playlists and multitracks (sections 4.3, 4.5). To make compelling examples, we
simultaneously introduce transitions, filters, and
properties (sections 4.4, 4.6, 4.7). Finally, we describe the interface authors use
to render their programs into traditional video files (@secref["overview-rendering"]).

@section[#:tag "overview-functions"]{Functions and binding}

Function and module scope are similar in Video. They both
lift definitions and describe a video with the remaining
expressions. The main difference is that modules provide a
video, while functions return one.@note{It is also possible
 to write functions for non-videos, we leave the details to
 the documentation.} In effect, Video modules are first-order
entities that can be compiled separately.

The implementation for @racket[conference-talk], which is
also written in Video, is shown in
@figure-ref["video-functions"]. Line 1-3 shows the function
header. While the syntax looks similar to Racket's function
syntax, the rest of the body (lines 4-21) is different. As
with modules, functions in Video are declarative and lift
definitions. In particular, line 4 is the producer returned
by this function. This figure also introduces
@racket[define*]. Rather than creating a recursive
definition, @racket[define*] replaces any old binding with
that name with a new one. Video programmers use this to
build up large objects from the inside out like using
definition chaining. The rest of the syntax in this figure
are video-specific. The remaining subsections show how these
forms combine to create a conference video.

@figure["video-functions" "Some Text"]{
 @racketblock[
@#,exact{1} (code:comment "Describes an edited conference video with appropriate feeds")
@#,exact{2} (code:comment "Producer Producer Producer Positive-Integer -> Producer")
@#,exact{3} (define (conference-talk video slides audio offset)
@#,exact{4}   (multitrack _ clean-audio)
@#,exact{5}   (code:comment "where")
@#,exact{6}   (define clean-audio (playlist (blank offset)
@#,exact{7}                                 (attach-filter audio
@#,exact{8}                                                (envelope-filter 50 #:direction 'in)
@#,exact{9}                                                (envelope-filter 50 #:direction 'out))))
@#,exact{10}  (define* _ (multitrack (blank #f)
@#,exact{11}                         (composite-transition 0 0 1/4 1/4)
@#,exact{12}                         slides
@#,exact{13}                         (composite-transition 1/4 0 3/4 1)
@#,exact{14}                         video
@#,exact{15}                         (composite-transition 0 1/4 1/4 3/4)
@#,exact{16}                         (image "logo.png" #:length (producer-length talk))))
@#,exact{17}  (define* _ (playlist (image "splash.png" #:length 100)
@#,exact{18}                       (fade-transition #:length 50)
@#,exact{19}                       _
@#,exact{20}                       (fade-transition #:length 50)
@#,exact{21}                       (image "splash.png" #:length 100))))]}

@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for a
Video program. A producer is, essentially, a data structure that can be coerced
into some sort of multimedia object: audio clips, video clips, pictures, and so on.
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
    (t# "clip" 4)
    (t# "clip" 5)
    (t# "clip" 6)
    (ellipses)
    (t# "clip" 18)
    (t# "clip" 19))))

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

A video is usually a composition of many producers. Video
provides two main ways for combining them: 
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler of the two compositing form.
Playlists are syntactically similar to Racket lists. Any producer
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
             (t# "clip" 4)
             (t# "clip" 5)
             (ellipses)
             (t# "clip" 18)
             (t# "clip" 19)
             (t# "clip" 20)
             (t# "clip" 21)
             (ellipses)
             (t# "clip" 45)
             (t# "clip" 46))))

@margin-note{@TODO{Specify works where cut is. But I really did
 mean cut. As in, I'm cutting a clip. I suspect the origin of
 the word comes from when movies were literal film, and clips were literally cut.}}
Developers @TODO{@emph{cut}} playlists, and in general producers, to desired lengths with the
@racket[#:start] and @racket[#:end] keywords. This capacity is
included because video recordings
frequently start before a talk begins and end after the
talk finishes:

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
             splash
             (t# "clip" 5)
             (t# "clip" 6)
             (ellipses)
             (t# "clip" 45)
             (t# "clip" 46)
             splash
             (ellipses))))

This example also introduces @racket[define]. Unlike
Racket's definitions or Haskell's @tt{where}-bound ones,
variables defined this way are available in the entire
surrounding module or function scope. Thus, the example
would yield the same result if the definition were at the
top of the code sequence.

@section{Transitions} Jumping from one producer in a
playlist to another can be jarring. Movie modules frequently reduce
this effect with @emph{transitions}: fading, swiping, etc.
These transitions merge the two adjacent clips in a playlist and
are placed directly inside of (possibly implicit) playlists. Using the
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
             splash
             (t# "trans" 9)
             (t# "trans" 11)
             (ellipses)
             (t# "trans" 31)
             (t# "trans" 10)
             splash
             (ellipses))))

Every transition in a playlist actually shortens the length
of the playlist, because transitions produce one clip for
every two clips they consume. Additionally, playlists may
contain multiple transitions. Such playlists still specify a unique
behavior because playlist transitions are
associative operations. Thus, multiple transitions placed in a
single playlist describe the desired clip without any
surprises.

@section{Multitracks}

Unlike playlists, multitracks play producers in parallel. As
with playlists however, they employ transitions to composite
their producers.

@(compound-paragraph
  (style #f '())
  (list
   @para{Syntactically, multitracks are similar to playlists. The
 @racket[multitrack] form accepts a list of producers and
 creates a new multitrack producer. Again, transitions are
 syntactically placed in the list to combine tracks:}
@(nested (split-minipage
  @racketblock[(multitrack
                (clip "slides.mp4")
                (composite-transition 0 0 1/4 1/4)
                talk)]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "pip" 2)
             (t# "pip" 3)
             (t# "pip" 4)
             (t# "pip" 5)
             (ellipses)))))
@para{This example uses @racket[composite-transition], which
places one producer on top of the other. The four constants
specify the coordinates of the top-left corner of the
producer and the screen space that the top producer takes.
Here, the producer following the transition appears in the
top-left hand of the screen and takes up one quarter of the width
and height of the screen.}))

@(compound-paragraph
  (style #f '())
  (list
   @para{
Transitions within multitracks are not associative; instead,
multitracks interpret transitions in left to right order.
Videos that require a different evaluation order can embed a
multitrack inside of a multitrack, because multitracks are
themselves producers. Using multiple transitions allow
producers to appear side by side rather than just on top of
each other. Modifying the running example from
@secref["impl-trans"] as follows describes a conference video where the
recording of the presenter goes in the top left while the
slides go on the right:}
   (nested (split-minipage
  @racketblock[(multitrack
                (blank #f)
                (composite-transition 0 0 1/4 1/4)
                (clip "slides.mp4")
                (composite-transition 1/4 0 3/4 1)
                talk)]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "npip" 2)
             (t# "npip" 3)
             (t# "npip" 4)
             (t# "npip" 5)
             (ellipses)))))
   @para{This example modifies the previous example by adding a
second @racket[composite-transition] and placing the
slides and the recording over a single blank producer. Blank
producers are simply empty producers that act as either a
background for a multitrack or a filler for a playlist. In
this case, the blank producer is providing a background that
the slides and camera feeds appear on.}))

@section{Filters}

Filters are similar to transitions, but they modify the
behavior of only a single producer. For example, filters can
remove the color from a clip or change a producer's aspect
ratio. Thus, filters are functions from producers
to producers.

Conference recordings frequently capture audio and video on
separate tracks. Before splicing the tracks together, a
developer may add an envelope filter to provide a fade
effect. Filters can be applied directly as
functions or attached to producers with either the
@racket[#:filters] keyword or the @racket[attach-filter]
function. Here is an example of a filter being attached to an
audio track that is itself composited with
the video of the talk above, @racket[composited-talk]:

@(split-minipage
  #:split-location 0.58
  @racketblock[(multitrack
                composited-talk
                (clip "0000.wav"
                      #:filters (list
                                 (envelope-filter 50 #:direction 'in)
                                 (envelope-filter 50 #:direction 'out))))
               (define composited-talk #,elided)]
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
    (t# "npip" 5)
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
   @para{The properties API comes with two functions:}
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
                  (image "logo.png" #:length (get-property talk 'length)))]
    (centered (make-playlist-timeline
               #:end #t
               (ellipses)
               (t# "nlpip" 2)
               (t# "nlpip" 3)
               (t# "nlpip" 4)
               (t# "nlpip" 5)
               (ellipses))))))

@section[#:tag "overview-rendering"]{From Programs to Videos}

Video programs may describe standalone modules or pieces of
a larger production. In the first case, users give Video
modules to a renderer that either plays the videos on a
screen, or saves them to a file. In the second case, other modules import the
video defined by the target module.

A renderer converts standalone Video programs to traditional
videos. Having a dedicated rendering pass allows developers
to set various properties such as aspect ratio, frame rate,
and even output format. The simplest renderer,
@racket[preview-video], consumes a path to a Video module,
and plays the video in a newly opened window. Here is an
example of a conference talk given to @racket[preview-talk]:

@(split-minipage
  (centered @racketinput[(preview-video "talk.vid")])
  (centered (scale (bitmap "res/talk-preview.png") 0.08)))

As with functions, Video modules compose to form larger
programs. Each Video module an identifier named
@racket[vid]. When evaluated, this @racket[vid] identifier
is the video described by that module. Thus, another module
can use the video by requiring the appropriate module, and
put the @racket[vid] identifier where appropriate.
Alternatively, the @racket[include-video] form does both
steps at the same time.

The left half of @figure-ref["video-use"] shows a program
split into multiple modules, while the right half is the
resulting video. This program describes the running
conference talk example, but now with an additional splash
screen on the front.@note{Imagine if you will, an
 organization rebroadcasting the video, but putting their own
 logo in the front.} The first line displays the additional
logo, while the second line is the video @tt{talk.vid} describes.

@(split-minipage
  @racketmod[video
             (image "splash.png" #:length 100)
             (include-video "talk.vid")]
  (centered (make-playlist-timeline
             #:end #t
             splash2
             (ellipses)
             splash2
             splash
             splash
             (ellipses))))

