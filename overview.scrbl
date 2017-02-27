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


@figure["video-script" @list{A first Video script}]{
@;%
@(begin
#reader scribble/comment-reader
@(racketmod
 video

(image "splash.png" #:length 100)

(fade-transition #:length 50)

(multitrack (blank #f)
            (composite-transition 0 0 1/4 1/4)
	    slides
	    (composite-transition 1/4 0 3/4 1)
	    presentation
	    (composite-transition 0 1/4 1/4 3/4)
	    (image "logo.png" #:length (producer-length talk)))

(code:comment "where")
(define slides
  (clip "slides05.MTS" #:start 2900 #:end 80000))

(define presentation
  (playlist (clip "vid01.mp4")
            (clip "vid02.mp4")
	    #:start 3900 #:end 36850))

(fade-transition #:length 50)

(image "splash.png" #:length 100)
))
@;%
 @exact{\vspace{0.3cm}} 
  @(centered
   rcon-timeline)
}

The literature survey suggests that non-linear
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
auxiliary definitions. It denotes a Racket module that exports a single item: a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. Another way is to import it into a second
Video module and to incorporate it into a larger video. 

@Figure-ref{video-script} displays a simple Video script. It consists of five
expressions, each describing a piece of a video clip. Right below the third
part of the video description, it also contains two definitions, which
introduce one name each so that the preceding @tt{multitrack} description
does not become too deeply nested. When the renderer turns this script into
an actual video, it turns the five pieces into sequence of images, taking
into account the transitions between the first and second fragment and the
fourth and the fifth. 

@figure["video-example" @list{A Video description of a conference talk}]{
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
   rcon-timeline)
}

In essence, the script in @figure-ref{video-script} assembles the visual
part of a simple conference video. What is missing, is the audio
part. Naturally, a Video programmer should abstract over this process, plus
the audio processing, and create a suitable library.
@Figure-ref["video-example"] shows what more or less the same script looks
like after a Video programmer has encapsulated an abstraction over the
script in @figure-ref{video-script} as a utility library. This program use
the imported @tt{conference-talk} function to combine a recording of the
speaker, a capture of the slides, and the audio. It even begins and ends
the video with the conference logo.  As mentioned, the first line of the
program specifies that this module is written in the Video language. Next,
the second line imports the library that defines the
@racket[conference-talk] function. The third line produces the video that this
module describes. Finally, the
remainder is a sequence of definitions that introduce auxiliary functions
and constants. 

@Figure-ref{video-functions} shows the essence of the utility library, also
written as a Video module. Explaining its constructing introduces enough of
the Videos primitives and combinators to get a sense of what the rest of
the language looks like. But first we explain Video's primary linguistic
mechanisms, modules and functions (@secref["overview-functions"]).  Next,
we describe basic producers (@secref["overview-simple"]): images, clips,
colors and so on. Then, we discuss the basics of how to combine these
producers into playlists and multitracks (sections 4.3, 4.5). To make
compelling examples, we introduce transitions, filters, and properties
(sections 4.4, 4.6, 4.7). Finally, we describe the interface for rendering
programs as traditional video files (@secref["overview-rendering"]).

@; -----------------------------------------------------------------------------
@section[#:tag "overview-functions"]{Essential Video}

Video inherits Racket's module and function syntax, but enforces different
scoping rules and assigns different meaning. Function and module scope are
similar in Video. In both cases, the definitions are valid in the entire
scope---that is, the entire module or the entire function body.  The
remaining expressions describe a video playlist. The semantic difference is
that modules @tt{provide} a video, while functions return one. Furthermore,
Video modules are first-order entities that can be compiled separately,
while functions are actually first-class values. 

Take a second look at the implementation for @racket[conference-talk],
shown in @figure-ref["video-functions"]. Line 1-3 shows the function
header.  The rest of the code describes the function body (lines
4-21). Functions in Video are declarative; in particular, line 4 is the
producer returned by this function. The function definition also introduces
the @racket[define*] syntax. Rather than creating a recursive function definition,
@racket[define*] replaces any binding with that name with a new
one. Video scripts use this binding form to build up large objects from the inside
out using definition chaining. 

The rest of the syntax in this figure is video-specific and explained in
the remaining subsections. The latter use the @tt{conference-talk} function
as an example to motivate essential Video forms.

@(define *line-no 0)
@(define (line-no)
   (set! *line-no  (+ *line-no 1))
   (define line-no (format (if (< *line-no 10) "0~a" "~a") *line-no))
   @exact{\tt @line-no})

@figure["video-functions" @list{A Video function definition}]{
 @racketmod[
video

@#,line-no[] (provide conference-talk)
@#,line-no[]
@#,line-no[] (code:comment "Describes an edited conference video with appropriate feeds")
@#,line-no[] (code:comment "Producer Producer Producer Positive-Integer -> Producer")
@#,line-no[] (define (conference-talk video slides audio offset)
@#,line-no[]   (multitrack _ clean-audio)
@#,line-no[]   (code:comment "where")
@#,line-no[]   (define clean-audio (playlist (blank offset)
@#,line-no[]                                 (attach-filter audio
@#,line-no[]                                                (envelope-filter 50 #:direction 'in)
@#,line-no[]                                                (envelope-filter 50 #:direction 'out))))
@#,line-no[]  (define* _ (multitrack (blank #f)
@#,line-no[]                         (composite-transition 0 0 1/4 1/4)
@#,line-no[]                         slides
@#,line-no[]                         (composite-transition 1/4 0 3/4 1)
@#,line-no[]                         video
@#,line-no[]                         (composite-transition 0 1/4 1/4 3/4)
@#,line-no[]                         (image "logo.png" #:length (producer-length talk))))
@#,line-no[]  (define* _ (playlist (image "splash.png" #:length 100)
@#,line-no[]                       (fade-transition #:length 50)
@#,line-no[]                       _
@#,line-no[]                       (fade-transition #:length 50)
@#,line-no[]                       (image "splash.png" #:length 100))))]}

@; -----------------------------------------------------------------------------
@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for a
Video program. A producer evaluates to a data structure that can be coerced
into some sort of multimedia object: audio clips, video clips, pictures, and so on.
Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers still.

The simplest type of producer, clips, incorporate traditional
video files. The clip producer converts the file into a sequence
of frames. Developers use clips to import recordings, such as
a conference talk, into their scripts:

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

Unlike clips, images are producers of an infinite stream of
frames. Video's combination forms truncate these streams to
fit the length other producers. Additionally,
developers can use the @racket[#:length] keyword when they
want a specific number of frames, such as creating an intro
sequence to a conference:

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

The playlist is the simpler of the two compositing form.
Playlists are syntactically similar to Racket lists. Any producer
can be put in a playlist including another playlist. Each
clip in the playlist plays in succession. Frequently, video
cameras split recordings into multiple files. With
playlists, developers can easily stitch these files together
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
             (t# "clip" 18)
             (t# "clip" 19)
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
talk finishes:
@;
@(split-minipage
  @racketmod[
video 

(image "logo.jpg" #:length 100)
talk
(image "logo.jpg" #:length 100)

(define talk
 (playlist (clip "talk00.MTS")
           (clip "talk01.MTS")
	   #:start 100
	   #:end 8000))
]
  (centered (make-playlist-timeline
             #:end #t
             splash
             (ellipses)
             (t# "clip" 5)
             (t# "clip" 6)
             (ellipses)
             (t# "clip" 45)
             (t# "clip" 46)
             splash
             (ellipses))))
@;
Recall that while @racket[define] is located below the description of the
video, it is in scope of the expression. 

@section{Transitions}

Jumping from one producer in a playlist to another can be rather
jarring. Scripts can reduce this effect with @emph{transitions}: fading,
swiping, etc.  These transitions merge the two adjacent clips in a playlist
and are placed directly inside of (possibly implicit) playlists. Using the
above example, fade transitions are used to smooth the transition from logo
to video:

@(split-minipage
    #:split-location 0.45
@racketmod[
video 

(image "splash.png" #:length 100)
(fade-transition #:length 50)
talk
(fade-transition #:length 50)
(image "splash.jpg" #:length 100)

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

Multitracks play producers in parallel. Like playlists, they employ
transitions to composite their producers.

Syntactically, multitracks are similar to playlists. The
 @racket[multitrack] form consists of a sequence of producers and
 creates a new multitrack producer. Again, transitions are
 included within the sequence to combine tracks:
@;
@(nested (split-minipage
  @racketmod[
video

(multitrack
 (clip "slides.mp4")
 (composite-transition 0 0 1/4 1/4)
 talk)

(define talk #,elided)

]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "pip" 2)
             (t# "pip" 3)
             (t# "pip" 4)
             (t# "pip" 5)
             (ellipses)))))
@;
This example uses @racket[composite-transition], which
places one producer on top of the other. The four constants
specify the coordinates of the top-left corner of the
producer and the screen space that the top producer takes.
Here, the producer following the transition appears in the
top-left hand of the screen and takes up one quarter of the width
and height of the screen.

Transitions within multitracks are not associative; instead,
multitracks interpret transitions in left to right order.
Videos that require a different order can embed a
multitrack inside of a multitrack, because multitracks are
themselves producers. Using multiple transitions allow
producers to appear side by side rather than just on top of
each other. Modifying the running example from
@secref["impl-trans"] as follows describes a conference video where the
recording of the presenter goes in the top left while the
slides go on the right:
@;
@(nested (split-minipage
  @racketmod[
video

(multitrack
 (blank #f)
 (composite-transition 0 0 1/4 1/4)
 (clip "slides.mp4")
 (composite-transition 1/4 0 3/4 1)
 talk)

(define talk #,elided)

]
  (centered (make-playlist-timeline
             #:end #t
             (ellipses)
             (t# "npip" 2)
             (t# "npip" 3)
             (t# "npip" 4)
             (t# "npip" 5)
             (ellipses)))))
@;
This example modifies the previous example by adding a
second @racket[composite-transition] and placing the
slides and the recording over a single blank producer. Blank
producers are empty slides that act as either a
background for a multitrack or a filler for a playlist. In
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
filters or attach them to producers with either the
@racket[#:filters] keyword or the @racket[attach-filter]
function. Here is an example of a filter being attached to an
audio track that is itself composited with
the video of the talk above, @racket[composited-talk]:

@(split-minipage
  #:split-location 0.58
  @racketmod[
video

(multitrack
 composited-talk
 (clip "0000.wav"
 #:filters (list
            (envelope-filter 50 #:direction 'in)
 	    (envelope-filter 50 #:direction 'out))))
            		     
(define composited-talk #,elided)
]
@;
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
Explicit properties provide a protocol fo
 communicating information from one clip to another. Implicit
 properties exist for the same purpose except that they store
 information that is implicitly associated with a producer,
 such as its length. For example, a conference video may have
 to come with a watermark that is the same length as the
 caputured conference talk. Here is a function that performs this
 operation:
@;
@(split-minipage
@racketmod[
video

(multitrack
 (blank #f)
 (composite-transition 0 0 1/4 1/4)
 (clip "slides.mp4")
 (composite-transition 1/4 0 3/4 1)
 talk
 (composite-transition 0 1/4 1/4 3/4)
 (image "logo.png" #:length (get-property talk 'length)))

(define talk #,elided)
]
    (centered (make-playlist-timeline
               #:end #t
               (ellipses)
               (t# "nlpip" 2)
               (t# "nlpip" 3)
               (t# "nlpip" 4)
               (t# "nlpip" 5)
               (ellipses))))

@; -----------------------------------------------------------------------------
@section[#:tag "overview-rendering"]{From Programs to Videos}

A Video module may be incorporated into a program or understood as a stand-alone
program. In the first case, another Video or Racket module may
@racket[require] the Video module and incorporate its export into its own
code.  In the second case, a user can hand the Video script to a renderer
that either plays the video on a screen or saves it to a file.

By default, a Video module @racket[provide]s a single identifier:
@racket[vid]. This identifier is bound to the data structure that
represents the module's entire playlist.  Another module
can import @racket[vid] and use it wherever appropriate.

Since Video modules may import several other modules, Racket's inherited
@racket[require] form is too cumbersome to use. Every @racket[require]
specification would have to prefix @racket[vid] in a distinct manner and
then refer to it via this prefix. To eliminate this programming pattern,
the Video language comes with  the @racket[include-video] form. It
simultaneously @racket[require]s a module and evaluates the imported
@racket[vid] identifier, evaluating to the data structure representation of
the video. 

For an example, consider this two-line module and what it denotes: 
@;
@(split-minipage
  @racketmod[
video

(image "splash.png" #:length 100)
(include-video "talk.vid")   
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
incorporates a Video module named @filepath{talk.vid}. 

A renderer converts Video scripts to traditional
videos. Having a dedicated rendering pass allows users
to set various visual properties such as aspect ratio, frame rate,
and even output format separately. The simplest renderers, dubbed
@racket[render] and @racket[preview], are functions that consume a producer
and display it in a separate window. At DrRacket's REPL, developers can
apply this function directly: 
@;
@(split-minipage
  (centered @racketinput[(preview vid)])
  (centered (scale (bitmap "res/talk-preview.png") 0.08)))
@;
While @racket[render] just displays the video, @racket[preview] adds
playback controls. 

Another renderer, called @racket[preview-video] is a function that consumes
a path to a Video script and plays it in a newly opened window:
@;
@(split-minipage
  (centered @racketinput[(preview-video "talk.vid")])
  (centered (scale (bitmap "res/talk-preview.png") 0.08)))
@;
This functionality is also available outside of the IDE so that
non-programmers may view the videos, too. 

@; -----------------------------------------------------------------------------
@section[#:tag "effectiveness"]{Effectiveness}

Two of the authors have been involved in the production of a video channel
for a developer conference. They report that creating Video and compositing
the videos for one conference took less time than manually editing the
videos for another, comparable conference (same number of talks, same
nature of talks, etc).

@emph{The submission omits the names of the authors and the conference
details to remain doubly blind. Since this submission is a pearl, we do not
consider this quasi-evaluation of Video critical to the acceptance decision.}
