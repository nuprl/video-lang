#lang scribble/sigplan

@require[scriblib/figure
         (except-in scribble/core table paragraph)
         (except-in scribble/manual cite)
         scribble/example
         pict
         (prefix-in pict: pict/code)
         (except-in pict/code typeset-code code)
         racket/format
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)

@title[#:tag "overview"]{The Design of Video}

The way producers create videos is at the heart of the Video
language. They need tools that allow them to describe the
video while also automating appropriate work. An embedded
DSL solves this task by providing creators with a
composeting specific declarative language that also has the
full power of its host language.

This section describes the language design, and how its
users turn programs into Video files. To accomplish this
goal, we split this section into the following parts: First,
we give atomic constructors and a simple program
(@secref["overview-simple"]). Next, the API for combining
video clips is described in @secref["overview-composite"].
We delay the discussion of how these compose to
@secref["case-study"]. Finally,
@secref["overview-rendering"] illustrates the users
perspective on previewing and rendering videos.

@section[#:tag "overview-simple"]{A Simple Video}

The @emph{producer} is the basic building block for Videos.
A producer is any data that can be coerced into a
video---audio clips, video clips, pictures, and even solid
colors. A Video program, composites all of the producers
into a single producer, that a render converts into a
traditional video file.

@;{
@figure["hello-color" "A Video Program (top) and output (bottom)"]{
 @hello-green}
 }

@Figure-ref["hello-color"] shows a sample program in Video
(top half), and the video it produces (bottom half). The
first line---@code["#lang video"]---declares that the file
describes a video. Unlike @code["#lang racket"] programs
that are run for their effects, programs written in Video
are declarative. Every top-level expression evaluates to a
producer, and becomes part of the rendered video.

@codeblock{
 #lang video
 (color "green")}

@centered[
 @(scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 150)]

The only other expression in this program is
@racket[(color "green")]. It creates an atomic producer that
represents a solid green image. In general, @racket[color]
creates a producer from any string or color object.

@(compound-paragraph
  (style #f '())
  (list
   @para{Another form, @racket[clip], creates a producer
 from a traditional video or audio file. For example, the
 following code evaluates to a producer that streams the file
 @tt["fire.mp4"]:}
   @racketblock[(clip "fire.mp4")]
   @centered[(scale-1080p (bitmap "res/fire.png") 150)]
  @para{The @racket[#:start] and @racket[#:end] keywords
 project a slice of the clip, create a producer that is a cut
 of the clip. Reusing the example from above:}
  @racketblock[(clip "fire.mp4" #:start 100 #:end 500)]
  @para{the resulting producer now includes only frames 100
 to 500. Finally, the @racket[clip] constructor works with all
 traditional forms of video and audio clips.}))

@(compound-paragraph
  (style #f '())
  (list
   @para{Pictures have their own constructor,
 @racket[image]. Unlike clips, images do not have an implicit
 start and end time. Rather, they can fill as much or little
 time as needed. If the pictures display time is important,
 creators use @racket[#:length] to construct a producer with
 that particular length. The image:}
   @racketblock[(image "dragon.png" #:length 50)]
   @centered[(scale-1080p (bitmap "res/dragon.png") 150)]
   @para{for example, remains on the screen for 50 frames}))

@(compound-paragraph
  (style #f '())
  (list
   @para{The
 final important form is @racket[blank]. Like @racket[image],
 it takes a @racket[#:length]
 parameter:}
   @racketblock[(blank #:length 49)]
   @para{A blank is equivalent to inserting a completely transparent
 color. Developers use it when they need a producer that takes
 time, but does not produce any media.}))

@section[#:tag "overview-composite"]{Videos Compositing}

A video is merely a composition of many clips. Video
provides two main ways for combining videos: @emph{
 playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel. Both data types additionally rely on
@emph{transitions} and @emph{filters}. Transitions inform
playlists and multitracks how multiple clips merge, such as
fading or overlays. Filters, unlike transitions, change the
behavior of a single producer---change the color, change
playback speed, etc.

@(compound-paragraph
  (style #f '())
  (list
   @para{@paragraph{Playlists} The playlist is the simpler
 video composite form. They are syntactically identical to
 lists. The following video:}
   @racketblock[(list (image "dragon.png")
                      (clip "fire.mp4"))]
   @centered[(make-playlist-timeline
              (clip-scale (bitmap "res/dragon.png"))
              (ellipses)
              (clip-scale (bitmap "res/fire.png")))]
   @para{plays a clip of a dragon flying followed by another
 clip of fire. Standard list operations also work with playlists.}
   @racketblock[(define dragons (list (image "dragon.mp4")
                                      (clip "fire.mp4")))
                (define colors (list (color "red")
                                     (color "blue")))
                (append dragon-clips color-clips)]
   @centered[
 (make-playlist-timeline
  (clip-scale (bitmap "res/dragon.png"))
  (ellipses)
  (clip-scale (bitmap "res/fire.png"))
  (ellipses)
  (clip-scale (filled-rectangle 50 50 #:color "red"))
  (ellipses)
  (clip-scale (filled-rectangle 50 50 #:color "blue")))]))

@paragraph{Transitions} Foo

@racketblock[(list (image "dragon.png" #:length 50)
                   (swipe-transition #:direction 'bottom
                                     #:durration 25)
                   (clip "fire.png"))]
@centered[
 (let ([size (clip-scale (blank 1))])
   (make-playlist-timeline
    (clip-scale (bitmap "res/dragon.png"))
    (ellipses)
    (vc-append
     (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -1/3))
     (inset/clip (clip-scale (bitmap "res/fire.png")) 0 (* (pict-height size) -2/3) 0 0))
    (ellipses)
    (vc-append
     (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -2/3))
     (inset/clip (clip-scale (bitmap "res/fire.png")) 0 (* (pict-height size) -1/3) 0 0))
    (ellipses)
    (clip-scale (bitmap "res/fire.png"))))]

@paragraph{Multitracks} Bar

@racketblock[(multitrack
              (clip "fire.mpg")
              (composite-transition 0 0 1/2 1/2)
              (image "dragon.png"))]
@centered[
 (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                 (scale-1080p (bitmap "res/dragon.png") 75))]

@racketblock[(define bg (clip "fire.png"))
             (define dragon (image "dragon.png"))
             (define color (color "red"))
             (multitrack dragon color bg
              #:transitions
              (list
               (composite-transition 0 0 1/2 1/2
                                     #:top dragon
                                     #:bottom bg)
               (composite-transition 1/2 0 1/2 1/2
                                     #:top color
                                     #:bottom bg)))]
@centered[
 (rt-superimpose
  (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                  (scale-1080p (bitmap "res/dragon.png") 75))
  (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "blue") 75))]

@paragraph{Filters} Baz

@racketblock[(scale-filter (image "dragon.png") 1 9)]
@centered[
 (let ([size (scale (scale-1080p (blank 1) 150) 1 9)])
   (inset/clip
    (scale (scale-1080p (bitmap "res/dragon.png") 150) 1 9)
    0 (* (pict-height size) (+ -1/3 -1/9)) 0 (* (pict-height size) (+ -1/3 -1/9))))]

@paragraph{Properties and Dependent Clips}
@racketblock[(define fire-clip
               (set-property (clip "fire")
                             'bottom? #f))
             (multitrack
              fire-clip
              (composite-transition
               0
               (if (get-property dragon 'bottom?)
                   1/2 0)
               1/2 1/2)
              (image "dragon.png"))]
@centered[
 (lb-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                 (scale-1080p (bitmap "res/dragon.png") 75))]

@racketblock[(define fire-clip (clip "fire"))
             (multitrack
              fire-clip
              (composite-transition 0 0 1/2 1/4)
              (image "dragon.png"
                     #:length (/ (get-property dragon
                                               'length)
                                 2)))]
@centered[
 (let ([composite-pict (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                                       (scale-1080p (bitmap "res/dragon.png") 75))])
   (make-playlist-timeline
    (clip-scale composite-pict)
    (clip-scale (bitmap "res/fire.png"))))]

@section[#:tag "overview-rendering"]{From Programs to Videos}

@centered[(mod->pict "green.vid" "video" (clip "green"))]

@examples[#:label #f
 (eval:alts (require "demo.vid") (void))
 (eval:alts vid 42)]

@exec{raco video -h 1920 -w 1080 demo.vid}

@centered[(scale (bitmap "res/sample.png") 0.08)]
