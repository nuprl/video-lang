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

@title[#:tag "overview"]{The Design of Video}

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

The rest of this section presents the language constructs in
Video. We use simple videos of basic geometric shapes and
colors to aid readability. Readers interested a larger
example can skip to @secref["case-study"], which shows how
these constructs form actual conference videos. First, we
describe basic producers: images, blanks, colors and so on.
Then, we discuss the basics of how to combine these
producers into playlists and multitracks. To make compelling
examples, we simultaneously introduce transitions, filters,
and properties. Finally, we describe the interface authors
use to render their programs into traditional video files.

@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for
Video programs. A producer is any data that can be coerced
into video: audio clips, video clips, pictures, and even
some data structures. Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers still.

The simplest type of producer, color, is a
data structures for drawing
pictures@cite[slideshow-jfp]. A color producer creates a
clip of the specified color with an unspecified length:

@(split-minipage
  @racketblock[(color "green")]
  (centered
   (make-playlist-timeline
    #:end #f
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (ellipses))))

Alternatively, a length can be specified with the
@racket[#:length] keyword:

@(split-minipage
  @racketblock[(color "blue" #:length 3)]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue")))))

Another producer, @racket[clip], describes a
traditional video or audio file. Either the clip is imported
in its entirely, or the @racket[#:start] and @racket[#:end]
keywords project a slice of the clip into the program. If
used, the two keywords specify the initial and final frames respectively.
The @racket[#:length] keyword can be
used here when only the length of the clip is relevant.

@(split-minipage
  @racketblock[(clip "rect.mp4" #:start 100 #:end 107)]
  (centered
   (apply make-playlist-timeline
    #:end #t
    (build-list 8
                (λ (n)
                  (clip-frame (list-ref rect-clip (+ n 2))))))))

Individual pictures have their own producer: @racket[image]. Unlike
clips, images do not have an implicit start and end time.
Rather, like colors, they can fill as much or little time as
needed. If the pictures display time is important,
@racket[#:length] constructs a producer with a specific
length:

@(split-minipage
  @racketblock[(image "circ.png" #:length 3)]
  (centered (apply make-playlist-timeline #:end #t (make-list 3 (clip-frame circ-image)))))

The final important producer is @racket[blank]. Like
@racket[image], it takes a @racket[#:length] parameter. A
blank is equivalent to inserting a completely transparent
color. While blank is not useful on its own, it is useful
for creating playlists with an offset in their start time.

@(split-minipage
  @racketblock[(blank 5)]
  (centered
   (apply make-playlist-timeline
          #:end #t
          (make-list 5 (clip-frame (rectangle 50 50))))))

@section{Playlists}

A video is merely a composition of many clips. Video
provides two main ways for combining videos:
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler of the two compositing form.
They are syntactically similar to lists. Any producer can be
put in a playlist including another playlist. Each clip in
the playlist plays in succession:

@(split-minipage
  @racketblock[(playlist (image "circ.png" #:length 3)
                         (clip "rect.mp4"))]
  (centered (make-playlist-timeline
             #:end #t
             (clip-frame circ-image)
             (clip-frame circ-image)
             (clip-frame circ-image)
             (clip-frame (first rect-clip))
             (clip-frame (second rect-clip))
             (clip-frame (third rect-clip))
             (ellipses)
             (clip-frame (last rect-clip)))))

Inserting a blank into playlists creates placeholder frames
that do not have any content. This is used if they
playlist should have an offset at the start or a gap in the
middle:

@(split-minipage
  #:split-location 0.43
  @racketblock[(playlist (blank 2)
                         (image "circ.png" #:length 2)
                         (clip "rect.mp4"))]
  (centered (make-playlist-timeline
             #:end #t
             (clip-frame (rectangle 50 50))
             (clip-frame (rectangle 50 50))
             (clip-frame circ-image)
             (clip-frame circ-image)
             (clip-frame (first rect-clip))
             (clip-frame (second rect-clip))
             (clip-frame (third rect-clip))
             (ellipses)
             (clip-frame (last rect-clip)))))

Playlists act as an append operator for producers. As an
example, appending two playlists together is as simple as
putting them into a new playlist.

@(minipage
  @racketblock[(playlist shapes colors)
               (define shapes (playlist (image "circ.png") (clip "rect.mp4")))
               (define colors (playlist (color "red" #:length 1) (color "blue")))]
  (centered
   (make-playlist-timeline
    #:end #f
    (clip-frame circ-image)
    (clip-frame (first rect-clip))
    (clip-frame (second rect-clip))
    (ellipses)
    (clip-frame (ninth rect-clip))
    (clip-frame (tenth rect-clip))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "red"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (ellipses))))

This sample also introduces binding using @racket[define].
Unlike with Racket's @racket[define] keyword or Haskell's
@tt{where} keyword, variables bound this way are available
in the entire module or function they are defined in. Thus,
the example would yield the same result if the definition is
at the top of the code.

@section{Transitions}
Jumping from one producer in a playlist to another
is jarring. Movies frequently reduce this effect with
@emph{transitions}: fading, swiping, etc. These transitions merge
two adjacent clips in a playlist.

@(compound-paragraph
  (style #f '())
  (list
   @para{Transitions are placed directly inside playlists. The
 transition mixes the two adjacent clips in the list:}
   (split-minipage
    #:split-location 0.45
    @racketblock[(playlist (image "circ.png" #:length 8)
                           (swipe-transition
                            #:direction 'bottom
                            #:length 7)
                           (clip "rect.mp4" #:length 8))]
    (centered
     (let ([size (clip-scale (blank 1))])
       (make-playlist-timeline
        #:end #t
        (clip-frame circ-image)
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/8))
          (inset/clip (clip-scale (first rect-clip)) 0 (* (pict-height size) -7/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -2/8))
          (inset/clip (clip-scale (second rect-clip)) 0 (* (pict-height size) -6/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -3/8))
          (inset/clip (clip-scale (third rect-clip)) 0 (* (pict-height size) -5/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -4/8))
          (inset/clip (clip-scale (fourth rect-clip)) 0 (* (pict-height size) -4/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -5/8))
          (inset/clip (clip-scale (fifth rect-clip)) 0 (* (pict-height size) -3/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -6/8))
          (inset/clip (clip-scale (sixth rect-clip)) 0 (* (pict-height size) -2/8) 0 0)))
        (clip-frame
         (vc-append
          (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -7/8))
          (inset/clip (clip-scale (seventh rect-clip)) 0 (* (pict-height size) -1/8) 0 0)))
        (clip-frame (eighth rect-clip))))))
   @para{Every transition in a playlist actually shortens the length
 of the playlist, because transitions produce one clip for
 every two clips they consume. For example, the playlist
 above has six frames without its transition, rather than
 four:}
   (split-minipage
    #:split-location 0.35
    @racketblock[(playlist
                  (image "circ.png" #:length 8)
                  (clip "rect.mp4" #:length 8))]
  (centered
    (make-playlist-timeline
     #:end #t
     (clip-frame circ-image)
     (clip-frame circ-image)
     (ellipses)
     (clip-frame circ-image)
     (clip-frame circ-image)
     (clip-frame (first rect-clip))
     (clip-frame (second rect-clip))
     (ellipses)
     (clip-frame (sixth rect-clip))
     (clip-frame (seventh rect-clip))
     (clip-frame (eighth rect-clip)))))))

Playlists may contain multiple transitions. Videos that
contain this behavior are not ambiguous because playlist
transitions are associative operations. Multiple transitions
placed in a single playlist described the desired clip
without any surprises:

@(split-minipage
  #:split-location 0.4
  @racketblock[(playlist
                (image "circ.png" #:length 4)
                (swipe-transition #:direction 'bottom
                                  #:length 3)
                (color "blue" #:length 7)
                (swipe-transition #:direction 'top
                                  #:length 3)
                (clip "rect.mp4" #:length 4))]
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      #:end #t
      (clip-frame circ-image)
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -3/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -2/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -2/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -3/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -1/4) 0 0)))
      (clip-frame (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (first rect-clip)) 0 0 0 (* (pict-height size) -3/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -1/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (first rect-clip)) 0 0 0 (* (pict-height size) -2/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -2/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (first rect-clip)) 0 0 0 (* (pict-height size) -1/4))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -3/4) 0 0)))
      (clip-frame (second rect-clip))))))

@section{Multitracks}

Unlike playlists, multitracks play producers in parallel.
Like with playlists, transitions are used to composite the
tracks.

Syntactically, multiracks are similar to playlists. The
@racket[multitrack] form accepts a list of producers and
creates a new multitrack producer. Again, transitions show
up directly in the list to combine tracks:

@(split-minipage
  @racketblock[(multitrack
                (clip "rect.mp4")
                (composite-transition 0 0 1/2 1/2)
                (image "circ.png"))]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lt-superimpose (scale-1080p (first rect-clip)  150)
                     (scale-1080p circ-image 75)))
    (clip-frame
     (lt-superimpose (scale-1080p (second rect-clip)  150)
                     (scale-1080p circ-image 75)))
    (ellipses)
    (clip-frame
     (lt-superimpose (scale-1080p (ninth rect-clip)  150)
                     (scale-1080p circ-image 75)))
    (clip-frame
     (lt-superimpose (scale-1080p (last rect-clip)  150)
                     (scale-1080p circ-image 75))))))

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
themselves producers. Here is an example:

@(split-minipage
  @racketblock[(multitrack
                (clip "rect.mp4")
                (composite-transition 0 0 1/2 1/2)
                (multitrack
                 (image "circ.png")
                 (composite-transition 0 1/2 1/2 1/2)
                 (color "green")))]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lb-superimpose
      (lt-superimpose (scale-1080p (first rect-clip) 150)
                      (scale-1080p circ-image 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (clip-frame
     (lb-superimpose
      (lt-superimpose (scale-1080p (second rect-clip) 150)
                      (scale-1080p circ-image 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (ellipses)
    (clip-frame
     (lb-superimpose
      (lt-superimpose (scale-1080p (ninth rect-clip) 150)
                      (scale-1080p circ-image 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (clip-frame
     (lb-superimpose
      (lt-superimpose (scale-1080p (last rect-clip) 150)
                      (scale-1080p circ-image 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))))

An alternative is to apply multiple transitions to the same
track. To support this alternative, multitracks support the
@racket[#:transitions] keyword, which takes a list of
transitions. Each transition in this list composites a pair
of producers, top and bottom, in the multitrack. These
producers are specified with the @racket[#:top] and
@racket[#:bottom] keywords respectively, and must be
producers found in the multitrack.

@(split-minipage
  @racketblock[(multitrack circ blue-color green-color bg
                           #:transitions
                           (list
                            (composite-transition 0 0 1/2 1/2
                                                  #:top circ
                                                  #:bottom bg)
                            (composite-transition 1/2 0 1/2 1/2
                                                  #:top blue-color
                                                  #:bottom bg)
                            (composite-transition 0 1/2 1/2 1/2
                                                  #:top green-color
                                                  #:bottom bg)))
              (define bg (clip "rect.mp4"))
              (define circ (image "circ.png"))
              (define green-color (color "green"))
              (define blue-color (color "blue"))]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lb-superimpose
      (rt-superimpose
       (lt-superimpose (scale-1080p (first rect-clip) 150)
                       (scale-1080p circ-image 75))
       (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "blue") 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (clip-frame
     (lb-superimpose
      (rt-superimpose
       (lt-superimpose (scale-1080p (second rect-clip) 150)
                       (scale-1080p circ-image 75))
       (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "blue") 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (ellipses)
    (clip-frame
     (lb-superimpose
      (rt-superimpose
       (lt-superimpose (scale-1080p (ninth rect-clip) 150)
                       (scale-1080p circ-image 75))
       (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "blue") 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75)))
    (clip-frame
     (lb-superimpose
      (rt-superimpose
       (lt-superimpose (scale-1080p (last rect-clip) 150)
                       (scale-1080p circ-image 75))
       (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "blue") 75))
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))))

The @racket[#:transitions] keyword also applies to
playlists.
This allows abstracting over both playlist and multitrack
functions that insert transitions into both of these data
representations without having to bother with list
operations directly. Here is an example:

@(minipage
  @racketblock[(swiping-playlist (image "circ.png") (color "green"))
               (swiping-playlist (color "green") (clip rect))
               (define (swiping-playlist a b)
                 (playlist a b
                           #:transitions
                           (list (swipe-transition
                                  #:direction 'bottom
                                  #:length 3
                                  #:first a
                                  #:second b))))]
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      #:end #t
      (clip-frame circ-image)
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/4))
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 (* (pict-height size) -3/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -2/4))
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 (* (pict-height size) -2/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -3/4))
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 (* (pict-height size) -1/4) 0 0)))
      (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
      (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 0 0 (* (pict-height size) -1/4))
        (inset/clip (first rect-clip) 0 (* (pict-height size) -3/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 0 0 (* (pict-height size) -2/4))
        (inset/clip (second rect-clip) 0 (* (pict-height size) -2/4) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 0 0 (* (pict-height size) -3/4))
        (inset/clip (third rect-clip) 0 (* (pict-height size) -1/4) 0 0)))
      (clip-frame (fourth rect-clip))
      (ellipses)
      (clip-frame (ninth rect-clip))
      (clip-frame (last rect-clip))))))

@section{Filters}

Filters are similar to transitions, but they modify the
behavior of only a single producer. For example, filters can
remove the color from a clip or change a producer's aspect
ratio. Thus, filters themselves are functions from producers
to producers. As an example, the @racket[scale-filter]
filter scales a producer by the given width and height,
leading to a stretching effect. Here, the first integer
scales the width, while the second one scales the height:

@(split-minipage
  #:split-location 0.45
  @racketblock[(scale-filter (clip "rect.mp4"  1 3))]
  (centered
   (let ([size (scale (scale-1080p (blank 1) 150) 1 9)])
     (apply make-playlist-timeline
      #:end #t
      (append
       (build-list
        6
        (λ (n)
          (clip-frame
           (inset/clip
            (scale (scale-1080p (list-ref rect-clip n) 150) 1 9)
            0 (* (pict-height size) (+ -1/3)) 0 (* (pict-height size) (+ -1/3))))))
       (list (ellipses))
       (build-list
        2
        (λ (n)
          (clip-frame
           (inset/clip
            (scale (scale-1080p (list-ref rect-clip (+ n 7)) 150) 1 9)
            0 (* (pict-height size) (+ -1/3)) 0 (* (pict-height size) (+ -1/3)))))))))))

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
 communicate information from one clip to another. For example, a watermark can
 communicate whether it should be placed at the top or bottom of
 the screen:}
   (split-minipage
    @racketblock[(multitrack
                  rect-clip
                  (composite-transition 0 1/2 3/4 3/4)
                  (image
                   "circ.png"
                   #:length (* 6 (get-property rect-clip 'ratio))))

                 (define rect-clip
                   (set-property (clip "rect.mp4" #:length 6) 'ratio 1/2))]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lb-superimpose (scale-1080p (first rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame
     (lb-superimpose (scale-1080p (second rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame
     (lb-superimpose (scale-1080p (third rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame (fourth rect-clip))
    (clip-frame (fifth rect-clip))
    (clip-frame (sixth rect-clip)))))))

Implicit properties store innate information about a clip.
Returning to the watermark example, a multitrack can get the
length of its main clip. That multitrack can then display
that watermark for a portion of the clip:

@(split-minipage
  @racketblock[(multitrack
                rect-clip
                (composite-transition 0 1/2 3/4 3/4)
                (image
                 "circ.png"
                 #:length (* (get-property rect-clip 'ratio)
                             (get-property rect-clip 'length))))
               
               (define rect-clip
                 (set-property (clip "rect.mp4" #:length 6) 'ratio 1/2))]
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lb-superimpose (scale-1080p (first rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame
     (lb-superimpose (scale-1080p (second rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame
     (lb-superimpose (scale-1080p (third rect-clip) 150)
                     (scale-1080p circ-image 120)))
    (clip-frame (fourth rect-clip))
    (clip-frame (fifth rect-clip))
    (clip-frame (sixth rect-clip)))))))


@section[#:tag "overview-rendering"]{From Programs to Videos}

Video programs may describe standalone films or pieces of
a larger production. In the first case, users give Video
files to a renderer. In the second case, other modules
import Video modules before rendering.

A renderer converts standalone Video programs to traditional
videos. This renderer allows creators to set various
properties such as aspect ratio, frame rate, and even output
format. If no output format is selected, Video plays a
preview of the film. For example, here is the command to
preview a clip that displays a green background:

@(split-minipage
  (centered @exec{raco video -h 1920 -w 1080 --fps 48 demo.vid})
  (centered (scale (bitmap "res/sample.png") 0.08)))

Every Video program is usable inside of other Video
programs and general purpose Racket programs. Each module
in the language exports one @racket[vid] identifier, that
contains the described film.

The top half of @figure-ref["video-use"] shows the program
for the green video used above. The code to the right shows
the contents of the @racket[vid] structure. This video is
itself a producer and can be used in larger projects, shown
in the bottom half of @figure-ref["video-use"]. Video
provides @racket[include-video] to import video files into
larger contexts. This function imports the specified file
and places the @racket[vid] struct where it is placed.

@figure["video-use"
        "Video program for green clip (top) and another program using the green clip (bottom)."]{
 @(split-minipage
   #:split-location 0.4
   @(minipage
     #:size 0.5
     (filebox "green.vid"
              @racketmod[video
                         (color "green")]))
   (examples #:label #f
             (eval:alts (require "demo.vid") (void))
             (eval:alts vid (display "#<producer>"))))
  
 @exact{\vspace{0.2cm}}
 
 @(hline 500 0)
 
 @exact{\vspace{0.5cm}}
 
 @(split-minipage
   @racketmod[video
              (clip "rect.mp4")
              (include-video "green.vid" #:length 1)
              @image["circ.png"]]
   (centered
    (make-playlist-timeline
     #:end #f
     (clip-frame (first rect-clip))
     (clip-frame (second rect-clip))
     (ellipses)
     (clip-frame (eighth rect-clip))
     (clip-frame (last rect-clip))
     (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
     (clip-frame circ-image)
     (ellipses))))}

@(type-table `(blank (-> Number Producer))
             `(color (-> (U String (X Number Number Number)) Producer))
             `(clip (-> String Producer))
             `(image (-> String Producer))
             `(playlist (-> (List (U Producer (-> Producer Producer Producer)))
                            #\newline
                            #:transitions
                            (List (U/man (X (-> Producer Producer Producer) Producer Producer)
                                         #\newline
                                         (Ghost "\\texttt{\\#:transitions} [( ")
                                         (Const " \\mid ")
                                         (X (-> Producer Producer) Producer)))
                            #\newline
                            Producer))
             `(multitrack (-> (List (U Producer (-> Producer Producer Producer)))
                              #\newline
                              #:transitions
                              (List (U/man (X (-> Producer Producer Producer) Producer Producer)
                                           #\newline
                                           (Ghost "\\texttt{\\#:transitions} [( ")
                                           (Const " \\mid ")
                                           (X (-> Producer Producer) Producer)))
                              #\newline
                              Producer))
             `(get-property (-> Producer String Any))
             `(set-property (-> Producer String Any Producer))
             `(attach-filter (-> Producer (-> Producer Producer) Producer))
             `(grayscale-filter (-> Producer Producer))
             `(cut-filter (-> Number Number (-> Producer Producer)))
             `(scale-filter (-> Number Number (-> Producer Producer)))
             `(translate-filter (-> Number Number (-> Producer Producer)))
             `(fade-transition (-> Number (-> Producer Producer Producer)))
             `(swipe-transition (-> String Number (-> Producer Producer Producer)))
             `(composite-transition (-> Number Number Number Number
                                        #\newline
                                        (-> Producer Producer Producer))))
