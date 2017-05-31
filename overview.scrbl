#lang scribble/acmart

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
         "bib.rkt"
         #;(for-label video)]

@(current-code-font code-font)
@(get-current-code-font-size (λ () font-size))
@(current-code-line-sep code-line-sep)
@(define blank-clip (clip-scale (blank 1)))

@title[#:tag "overview"]{The Producers}

@(define *line-no 0)
@(define (line-no)
   (set! *line-no  (+ *line-no 1))
   (define line-no (format (if (< *line-no 10) "0~a" "~a") *line-no))
   @exact{\tt @line-no})

The literature survey in the preceding section suggests that non-linear
video editing distinctly separates the description of a
video clip from the rendering action on it. Specifically, an
editor (as in the tool) needs a description of what the final video
should look like in terms of the given pieces. The action of
rendering this video is a distinct second step.
Going from this assessment to a language design requires one
more idea: abstraction. For example, a description of a
video composition should be able to use a sequence
comprehension to apply a watermark to all images. Similarly,
a professional may wish to create one module per ICFP
presentation in order to make up a complete ICFP channel in a modular fashion.
And of course, the language must allow the definition and
use of functions because it is the most common form of
abstraction.

The Video language gets to the heart of the domain. Each Video program
is a complete module that intermingles descriptions of video clips and
auxiliary definitions. It denotes a Racket module that exports a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. A different way is to import it into a second
Video module and to incorporate its exported playlist into another video. 

@(set! *line-no 0)
@figure["video-example" @list{A Video description of a conference talk}]{
 @racketblock[
@#,line-no[] @#,hash-lang[] video
@#,line-no[]
@#,line-no[] (require "conference-lib.vid")
@#,line-no[]
@#,line-no[] (conference-talk video slides audio 125)
@#,line-no[] (code:comment @#,elem{where})
@#,line-no[] (define slides (clip "slides05.MTS" #:start 2900 #:end 80000))
@#,line-no[] (define video  (playlist (clip "vid01.mp4") (clip "vid02.mp4")
@#,line-no[]                          #:start 3900 #:end 36850))
@#,line-no[] (define audio  (playlist (clip "capture01.wav") (clip "capture02.wav")))]
 @exact{\vspace{0.3cm}} 
 @(centered
   rcon-timeline)
}


@Figure-ref{video-script} displays a simple Video script. It consists of five
expressions, each describing a piece of a video clip. Right below the third
part of the video description, it also contains two definitions, which
introduce one name each so that the preceding @code{multitrack} description
does not become too deeply nested. When the renderer turns this script into
an actual video, it turns the five pieces into sequence of images, taking
into account the transitions between the first and second fragment and the
fourth and the fifth. 

In essence, the script in @figure-ref{video-script} assembles the visual
part of a simple conference video. What is missing is the audio
part. Naturally, a Video programmer should abstract over this sequence of expressions, plus
the audio processing, and create a suitable library.
@Figure-ref["video-example"] shows what the same script roughly looks
like after a Video programmer has encapsulated an abstraction over the
script in @figure-ref{video-script} as a utility library. This program uses
the imported @code{conference-talk} function to combine a recording of the
speaker, a capture of the slides, and the audio.
As mentioned, line 1 of the
program specifies that this module is written in the Video language. Line 3
imports the library that defines the
@racket[conference-talk] function. Line 5 produces the video that this
module describes. Finally, the
remainder is a sequence of definitions that introduce auxiliary constants. 

@Figure-ref{video-functions} shows the essence of the
utility library, also written as a Video module. Explaining
its construction introduces enough of Video's primitives and
combinators to get a sense of what the rest of the language
looks like. First we explain Video's primary linguistic
mechanisms, modules and functions
(@secref["overview-functions"]). Then, we describe basic
producers (@secref["overview-simple"]): images, clips,
colors and so on, following up with a discussion of the
basics of how to combine these producers into playlists and
multitracks (sections 4.3, 4.5). To make compelling
examples, we introduce transitions, filters, and properties
(sections 4.4, 4.6, 4.7). Finally, we describe the interface
for displaying videos and rendering Video programs (@secref["overview-rendering"]).

@; -----------------------------------------------------------------------------
@section[#:tag "overview-functions"]{Essential Video}

Video modules consist of a series of interleaved
expressions, definitions, and import/export forms; functions
have the same shape as modules but without the import/export forms. Video
enforces different scoping rules, and assigns slightly
different meaning to these constructs, than Racket does. In both
cases, definitions are valid in the entire scope---that
is, the entire module or the entire function body. The
expressions must describe video playlists. Modules and functions 
differ in that the former @code{provide}s a video, while
the latter returns one. Furthermore, Video modules are
first-order entities that can be compiled separately, while
functions are actually first-class values.

@(set! *line-no 0)
@figure["video-functions" @list{A Video function definition}]{
 @racketblock[
@#,line-no[] @#,hash-lang[] video
@#,line-no[]
@#,line-no[] (provide conference-talk)
@#,line-no[]
@#,line-no[] (code:comment "Describes an edited conference video with appropriate feeds")
@#,line-no[] (code:comment "Producer Producer Producer Positive-Integer -> Producer")
@#,line-no[] (define (conference-talk video slides audio offset)
@#,line-no[]   (multitrack clean-video clean-audio)
@#,line-no[]   (code:comment "where")
@#,line-no[]   (define clean-audio
@#,line-no[]     (playlist (blank offset)
@#,line-no[]               (attach-filter audio
@#,line-no[]               (envelope-filter 50 #:direction 'in)
@#,line-no[]               (envelope-filter 50 #:direction 'out))))
@#,line-no[]   (define spliced-video
@#,line-no[]     (multitrack (blank #f)
@#,line-no[]                 (composite-transition 0 0 1/4 1/4)
@#,line-no[]                 slides
@#,line-no[]                 (composite-transition 1/4 0 3/4 1)
@#,line-no[]                 video
@#,line-no[]                 (composite-transition 0 1/4 1/4 3/4)
@#,line-no[]                 (image "logo.png" #:length (producer-length talk))))
@#,line-no[]   (define clean-video
@#,line-no[]     (playlist (image "splash.png" #:length 100)
@#,line-no[]               (fade-transition #:length 50)
@#,line-no[]               spliced-video
@#,line-no[]               (fade-transition #:length 50)
@#,line-no[]               (image "splash.png" #:length 100))))]}

Now, take a second look at
@figure-ref["video-functions"]. @nonbreaking{Lines 5 through 7} show the
function header. The rest of the code describes the function
body (lines 7--28). Functions in Video are declarative; in
particular, line 8 is the producer returned by this
function. The remaining subsections explain the Video
language in sufficient detail to understand the rest of this
code. Specifically, we explain individual features of the
language and how they improve the video editing process.

@; -----------------------------------------------------------------------------
@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for a
Video program. A producer evaluates to a data structure that denotes
some sort of multimedia object: audio clips, video clips, pictures, and so on.
Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers.

The simplest type of producer, @racket[clip], incorporates
traditional video files. The @racket[clip] producer converts
the file into a sequence of frames. Developers use
@racket[clip] to import recordings from files, such as a
conference talk, into scripts:

@(split-minipage
  @racketmod[ 
video

(clip "talk00.mp4")]
  (centered
   (make-playlist-timeline
    #:end #t
    (t# "clip" 4)
    (t# "clip" 5)
    (t# "clip" 6)
    (ellipses)
    (t# "clip" 18)
    (t# "clip" 19))))

Unlike @racket[clip], @racket[image] creates an infinite stream of
frames. Video's combination forms truncate these streams to
fit the length of other producers. Additionally,
developers can use the @racket[#:length] keyword when they
want a specific number of frames:

@(split-minipage
  @racketmod[
video 

(image "splash.png" #:length 3)]
  (centered (apply make-playlist-timeline #:end #t
                   (make-list 3 (clip-frame (bitmap "res/rcon.png"))))))

@section[#:tag "impl-trans"]{Playlists}

A video is usually a composition of many producers. Video
provides two main ways for combining them: 
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.
Any producer
can be put in a @racket[playlist], including another @racket[playlist]. Each
clip in the @racket[playlist] plays in succession. Frequently, video
cameras split recordings into multiple files. With
@racket[playlist], developers can easily stitch these files together
to form a single producer:

@(split-minipage
  @racketmod[
video 

(playlist (clip "talk00.MTS")
          (clip "talk01.MTS"))
]
  (centered (make-playlist-timeline
             #:end #t
             (t# "clip" 4)
             (t# "clip" 5)
             @;(ellipses)
             (t# "clip" 20)
             (t# "clip" 21)
             (ellipses)
             (t# "clip" 45)
             (t# "clip" 46)
	     (ellipses)
)))

Developers cut playlists and other producers to desired lengths with the
@racket[#:start] and @racket[#:end] keywords. This capability is
included because video recordings
frequently start before a talk begins and end after the
talk finishes; see @figure-ref["playlist-cut"]a.
Recall that while @racket[define] is located below the video description,
its scope includes the preceding expressions. 

@figure["playlist-cut" "(a). Example of cutting a playlist"]{
@(split-minipage
  #:split-location 0.43
  @racketmod[
video 

(image "logo.jpg" #:length 100)
talk
(image "logo.jpg" #:length 100)

(code:comment "where")
(define talk
 (playlist (clip "talk00.MTS")
           (clip "talk01.MTS")
           #:start 100
           #:end 8000))
]
  (make-playlist-timeline
   #:end #t
   splash
   (ellipses)
   (t# "clip" 5)
   (t# "clip" 6)
   (ellipses)
   (t# "clip" 45)
   (t# "clip" 46)
   splash
   (ellipses)))}

@section{Transitions}

Jumping from one producer in a playlist to another can be
rather jarring. Scripts can reduce this effect with @emph{
 transitions}: fading, swiping, etc. These transitions merge
the two adjacent clips in a playlist and are placed directly
inside of (possibly implicit) playlists. Expanding on the
example from @figure-ref["playlist-cut"]a, fade transitions
are used to smooth the transition from logo to the talk.
@Figure-ref["playlist-cut"]b illustrates this point.

@figure["playlist-cutb" "(b). Example with fading transition"
        #:continue? #t]{
@(split-minipage
    #:split-location 0.45
@racketmod[
video 

(image "splash.png" #:length 100)
(fade-transition #:length 50)
talk
(fade-transition #:length 50)
(image "splash.jpg" #:length 100)

(code:comment "where")
(define talk #,elided)

]
@; 
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
             (ellipses))))}

Every transition in a @racket[playlist] actually shortens the length
of its frame sequence, because transitions produce one clip for
every two clips they consume. Additionally, a @racket[playlist] may
contain multiple transitions. Such a @racket[playlist] still specifies a unique
behavior because @racket[playlist] transitions are
associative operations. Thus, multiple transitions placed in a
single @racket[playlist] describe the desired clip without any
surprises.

@section{Multitracks}

@figure["playlist-cutc" "(c). Example using multitracks"
        #:continue? #t]{
@(nested (split-minipage
  @racketmod[
video

(multitrack
 (clip "slides.mp4")
 (composite-transition 0 0 1/4 1/4)
 talk)

(code:comment "where")
(define talk #,elided)

]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "pip" 2)
             (t# "pip" 3)
             (t# "pip" 4)
             (t# "pip" 5)
             (ellipses)))))}

Multitracks play producers in parallel. Like playlists, they employ
transitions to composite their producers.
Syntactically, @racket[multitrack] is similar to @racket[playlist]. The
@racket[multitrack] form consists of a sequence of producers and
creates a new @racket[multitrack] producer. Again, transitions are
included within the sequence to combine tracks; see @figure-ref["playlist-cut"]c.
This example uses @racket[composite-transition], which
places one producer on top of the other. The four constants
specify the coordinates of the top-left corner of the
producer and the screen space that the top producer takes.
Here, the producer following the transition appears in the
top-left hand of the screen and takes up one quarter of the width
and height of the screen.

@figure["playlist-cutd" "(d). Example using inlined transitions in a multitracks"
        #:continue? #t]{
@(nested (split-minipage
  @racketmod[
video

(multitrack
 (blank #f)
 (composite-transition 0 0 1/4 1/4)
 (clip "slides.mp4")
 (composite-transition 1/4 0 3/4 1)
 talk)

(code:comment "where")
(define talk #,elided)

]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "npip" 2)
             (t# "npip" 3)
             (t# "npip" 4)
             (t# "npip" 5)
             (ellipses)))))}

Transitions within a @racket[multitrack] are not associative; instead,
@racket[multitrack] interprets transitions in left to right order.
Videos that require a different order can embed a
@racket[multitrack] inside of another one, because a @racket[multitrack] is
itself a producer. Using multiple transitions allow
producers to appear side by side rather than just on top of
each other. Expanding on the running example in @figure-ref["playlist-cut"]c,
@figure-ref["playlist-cut"]d describes a conference video where the
recording of the presenter goes in the top left while the
slides go on the right.
This example adds a
@racket[composite-transition] to the previous example and places the
slides and the recording over a single blank producer. Blank
producers are empty slides that act as either a
background for a @racket[multitrack] or a filler for a @racket[playlist]. In
this case, the blank producer is providing a background that
the slides and camera feeds appear on.

@section{Filters}

Filters are similar to transitions, but they modify the behavior of only
 one producer. In other words, filters are functions from producers to
 producers. Among other effects, filters can remove the color from a clip
 or change a producer's aspect ratio. Conference recordings frequently
 capture audio and video on separate tracks. Before splicing the tracks
 together, a developer may add an envelope filter to provide a fade effect
 for audio.

A script may use function application notation to apply
filters or attach them to producers with the
@racket[#:filters] keyword. @Figure-ref["playlist-cut"]e
shows an example of a filter being attached to an audio
track that is itself composited with the video of the composited talk in
@figure-ref["playlist-cut"]d.

@figure["playlist-cute" "(e). Example of adding audio tracks"
        #:continue? #t]{
@(split-minipage
  #:split-location 0.56
  @racketmod[
video

(multitrack
 composited-talk
 (clip "0000.wav"
  #:filters
    (list
     (envelope-filter 50 #:direction 'in)
     (envelope-filter 50 #:direction 'out))))

(code:comment "where")
(define composited-talk #,elided)
]
@;
  (vc-append
   25
   (vc-append 20
              (blank 1)
              (hc-append 10
                         (scale-to-fit (bitmap "res/sound-start.png") 90 30 #:mode 'distort)
                         (ellipses)
                         (scale-to-fit (bitmap "res/sound-end.png") 90 30 #:mode 'distort)))
   (make-playlist-timeline
    #:end #t
    (clip-scale (bitmap "res/rcon.png"))
    (ellipses)
    (t# "npip" 5)
    (ellipses)
    (clip-scale (bitmap "res/rcon.png")))))}

@section{Properties and Dependent Clips}

Producers use two types of properties to store information:
implicit and explicit properties. Implicit properties are
innate to clips, for example, length and dimensions.
Explicit properties must be added by the program itself.

The properties API comes with two functions:
@itemlist[

 @item{@racket[(set-property #,(emph "producer") #,(emph "key") #,(emph "value"))] creates
  an explicit property. It returns a new producer with @emph{key}
  associated with @emph{value}.} 

 @item{@racket[(get-property #,(emph "producer") #,(emph "key"))] returns
  the value associated with @emph{key}. If the property is set
  both implicitly and explicitly, the explicit property has priority.}

]
@;
Explicit properties provide a protocol for
 communicating information from one clip to another. Implicit
 properties exist for the same purpose except that they store
 information that is implicitly associated with a producer,
 such as its length. For example, a conference video may have
 to come with a watermark that is the same length as the
 captured conference talk. A script that performs this
 operation can be found in @figure-ref["playlist-cut"]f.

@figure["playlist-cutf" "(f). Example of adding a watermark"
        #:continue? #t]{
@(split-minipage
  #:split-location 0.6
@racketmod[
video

(multitrack
 (blank #f)
 (composite-transition 0 0 1/4 1/4)
 (clip "slides.mp4")
 (composite-transition 1/4 0 3/4 1)
 talk
 (composite-transition 0 1/4 1/4 3/4)
 (image "logo.png"
  #:length (get-property talk 'length)))

(code:comment "where")
(define talk #,elided)
]
(make-playlist-timeline
 #:end #t
 (ellipses)
 (t# "nlpip" 2)
 (t# "nlpip" 3)
 (t# "nlpip" 4)
 (t# "nlpip" 5)
 (ellipses)))}

@; -----------------------------------------------------------------------------
@section[#:tag "overview-rendering"]{From Programs to Videos}

A Video module may be incorporated into a program or understood as a stand-alone
program. In the first case, another Video or Racket module may
@racket[require] the Video module and incorporate its export into its own
code.  In the second case, a user can hand the Video script to a renderer
that either plays the video on a screen or saves it to a file.

By default, a Video module implicitly @racket[provide]s a
producer. Any module that wants to use this implicitly created
video imports it with the @racket[external-video] form.
For an example, consider this two-line module and what it
denotes:

@;
@(split-minipage
  @racketmod[
video

(image "splash.png" #:length 100)
(external-video "talk.vid")   
]

  (centered (make-playlist-timeline
             #:end #t
             splash2
             (ellipses)
             splash2
             splash
             splash
             (ellipses))))
@;
The module's first line sets up a splash screen, the second line
incorporates the external Video module.

@figure["overview-preview" "Previewing Videos"]{
@(split-minipage
  #:split-location 0.65
  #:indent-offset 0.2
  (nested (values #;larger @racketinput0[(preview
                                          (external-video "talk.vid"))]))
  (nested (values #;larger @racketinput0[(preview-video
                                          "talk.vid")])))
 @(blank 1 20)
 
@(scale (bitmap "res/talk-preview.png") 0.45)
  }

A renderer converts Video scripts to traditional videos.
Having a dedicated rendering pass allows users to set
various visual properties such as aspect ratio, frame rate,
and even output format separately. The simplest renderers,
dubbed @racket[render] and @racket[preview], are functions
that consume a producer and display it in a separate window.
At DrRacket's REPL, developers can apply this function
directly (left half of @figure-ref{overview-preview}).
While @racket[render] just displays the video,
@racket[preview] adds playback controls, and even gives
developers the ability to preview a video excerpt. Another
renderer, called @racket[preview-video] is a function that
consumes a path to a Video script and plays it in a newly
opened window (right half of
@figure-ref{overview-preview}). This functionality is
available outside of the IDE so that non-programmers may
view the videos, too.

@; -----------------------------------------------------------------------------
@section[#:tag "effectiveness"]{Effectiveness}

Leif Andersen, the first author, has
been involved in the production of a video channel
for RacketCon 2016. In Andersen's experience, creating Video and compositing
the videos for that conference took less time than manually editing the
videos for the previous year's conference (same number of talks, same
nature of talks, etc).
