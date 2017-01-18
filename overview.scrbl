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

The way filmmakers create videos is at the heart of the
Video language. They need tools that allow them to describe
the video while also automating appropriate work. An
embedded DSL solves this task; it provides creators with a
declarative language designed for composeting videos, and
also has the full power of its host language.

This section describes the language design, and how its
users turn programs into videos. To accomplish this goal, we
split this section into the following parts: First, we give
the atomic constructs of film (@secref["overview-simple"]).
Next, the API for combining video clips is described in
@secref["overview-composite"]. Finally,
@secref["overview-rendering"] illustrates the users
perspective on previewing and rendering videos.

@section[#:tag "overview-simple"]{A Simple Video}

The @emph{producer} is the basic building block for video
programs. A producer is any data that can be coerced into a
video---audio clips, video clips, pictures, and even solid
colors. Video programs composite all of the producers
into a single producer, which a render converts into a
traditional video file.

Every Video program begins with @code["#lang video"]. Unlike
traditional languages where expressions are run for their
effects, the remainder of the file is a video description.
The @racket[color] function creates a colored producer with
an unspecified length.

@racketmod[
 video
 (color "green")]
@centered[
 @(scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 150)]

Another form, @racket[clip], creates a producer from a
traditional video or audio file. Either the clip is imported
in its entirely, or the @racket[#:start] and @racket[#:end]
keywords project a slice of the clip into the program. If
used, these keywords specify the initial and final frames
that are are included.
@racketblock[(clip "fire.mp4" #:start 100 #:end 500)]
@centered[(scale-1080p (bitmap "res/fire.png") 150)]

Pictures have their own constructor: @racket[image]. Unlike
clips, images do not have an implicit start and end time.
Rather, they can fill as much or little time as needed. If
the pictures display time is important, @racket[#:length]
constructs a producer with a specified length.}
@racketblock[(image "dragon.png" #:length 50)]
@centered[(scale-1080p (bitmap "res/dragon.png") 150)]

The final important form is @racket[blank]. Like
@racket[image], it takes a @racket[#:length] parameter. A
blank is equivalent to inserting a completely transparent
color. Developers use it when they need a producer that
takes time, but does not produce any media.
@racketblock[(blank #:length 50)]

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
playback speed, etc. Finally, producer properties allow for
programs with dependent producers.

@paragraph{Playlists} The playlist is the simpler video
composite form. They are syntactically identical to lists.
Any producer can be put in a playlist. Each clip in the list will play in succession.

@racketblock[(list (image "dragon.png")
                   (clip "fire.mp4"))]
@centered[(make-playlist-timeline
           (clip-scale (bitmap "res/dragon.png"))
           (ellipses)
           (clip-scale (bitmap "res/fire.png")))]

Standard list operations also work with playlists. For
example, @racket[append] creates a new playlist comprised of
the playlists given to it.

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
  (clip-scale (filled-rectangle 50 50 #:color "blue")))]

@paragraph{Transitions} Jumping from one producer to another
is jarring. Movies frequently reduce this effect with
transitions---fading, swiping, etc. These transitions merge
two adjacent clips in a playlist.

Creators place transitions inside playlists directly. While
each transition is a producer, every playlist that a
transition is placed into actually shortens the length of
the playlist. For example, a transition with a length of 50
frames will make the playlist 25 frames shorter. Transitions
shorten playlists because they consume two frames from its
adjacent producers for every frame it produces.

@racketblock[(list (image "dragon.png" #:length 50)
                   (swipe-transition #:direction 'bottom
                                     #:durration 50)
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

Playlists can also have multiple transitions. Transitions,
however, are associative operations. Therefore multiple
transitions can be placed in a single playlist without any
unexpected effects.

@racketblock[(list (image "dragon.png" #:length 50)
                   (swipe-transition #:direction 'bottom
                                     #:durration 50)
                   (color "blue" #:length 100)
                   (swipe-transition #:direction 'top
                                     #:duration 50)
                   (clip "fire.png"))]
@centered[
 (let ([size (clip-scale (blank 1))])
   (make-playlist-timeline
    (clip-scale (bitmap "res/dragon.png"))
    (ellipses)
    (vc-append
     (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -1/2))
     (inset/clip (clip-scale (filled-rectangle 100 100 #:color "blue"))
                 0 (* (pict-height size) -1/2) 0 0))
    (ellipses)
    (clip-scale (filled-rectangle 100 100 #:color "blue"))
    (ellipses)
    (vc-append
     (inset/clip (clip-scale (bitmap "res/fire.png")) 0 0 0 (* (pict-height size) -1/2))
     (inset/clip (clip-scale (filled-rectangle 100 100 #:color "blue"))
                 0 (* (pict-height size) -1/2) 0 0))
    (ellipses)
    (clip-scale (bitmap "res/fire.png"))))]

@paragraph{Multitracks} More complicated constructs are
multitracks. Unlike playlists, multitracks play producers in
parallel. Like image layers, the renderer can only display
one track. Developers use transitions to composite the
tracks together.

Multiracks are syntactically similar to playlist. The
@racket[multitrack] form accepts a list of producers and
produces a new multitrack producer. Like playlists,
transitions are place directly in the list to combine
tracks.

@racketblock[(multitrack
              (clip "fire.mpg")
              (composite-transition 0 0 1/2 1/2)
              (image "dragon.png"))]
@centered[
 (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                 (scale-1080p (bitmap "res/dragon.png") 75))]

Transitions in multitracks are not associative. Multricks
interpret transitions in left to right order. Videos that
require a different evaluation order can embed a multitrack
inside of a multitrack. This is possible because, like
playlists, multitracks are themselves producers.

@racketblock[(multitrack
              (clip "fire.png")
              (composite-transition 0 0 1/2 1/2)
              (multitrack
               (image "dragon.png")
               (composite-transition 0 1/2 1/2 1/2)
               (color "green")))]
@centered[
 (lb-superimpose
  (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                  (scale-1080p (bitmap "res/dragon.png") 75))
  (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))]

A second alternative is to apply multiple transitions to the
same track. The @racket[#:transitions] keyword composites a
top and bottom producer in the multitrack.

@racketblock[(define bg (clip "fire.png"))
             (define dragon (image "dragon.png"))
             (define green-color (color "green"))
             (define red-color (color "red"))
             (multitrack dragon color bg
              #:transitions
              (list
               (composite-transition 0 0 1/2 1/2
                                     #:top dragon
                                     #:bottom bg)
               (composite-transition 1/2 0 1/2 1/2
                                     #:top red-color
                                     #:bottom bg)
               (composite-transition 0 1/2 1/2 1/2
                                     #:top green-color
                                     #:bottom bg)))]
@centered[
 (lb-superimpose
  (rt-superimpose
   (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                   (scale-1080p (bitmap "res/dragon.png") 75))
   (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "red") 75))
  (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))]

Racket's @racket[eq] function determines producer
equivalence. Multitracks are able to use this pattern
because producers are functional objects.

@paragraph{Filters} Unlike transitions, filters modify the
behavior of a single producer. Some things filters are
useful for include removing the color from a clip or even
changing a producer's aspect ratio.

All filters are functions with producers for the domain and
range: @mm{\mathit{filter} : @tt{producer}\:\: @tt{
  extra-args}\: \cdots\: \rightarrow\: @tt{producer}} For
example, the @racket[scale-filter] filter scales a producer
by the given width and height.

@racketblock[(scale-filter (image "dragon.png") 1 9)]
@centered[
 (let ([size (scale (scale-1080p (blank 1) 150) 1 9)])
   (inset/clip
    (scale (scale-1080p (bitmap "res/dragon.png") 150) 1 9)
    0 (* (pict-height size) (+ -1/3 -1/9)) 0 (* (pict-height size) (+ -1/3 -1/9))))]

@paragraph{Properties and Dependent Clips} Producers use
properties to store and retrieve information about other
producers. They additionally store both implicit and
explicit properties. Implicit properties are innate to a
clips, such as length and dimensions. Explicit properties
are added in the program.

The properties API has two calls:
@itemlist[
 @item{@racket[(set-property #,(emph "producer") #,(emph "key") #,(emph "value"))]---creates
  an explicit property. It returns a new producer with @emph{
   key} functionally set to @emph{value}.}
 @item{@racket[(get-property #,(emph "producer") #,(emph "key"))]---Returns
  the value associated with @emph{key}. If the property is set
  both implicitly and explicitly, the explicit property is returned.}]

Explicit properties provide a protocol for clips to
communicate information. For example, a watermark can
communicate if it should be placed at the top or bottom of
the screen.

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

Implicit properties store innate information about a clip.
Returning to the watermark example, a multitrack can get the
length of its main clip. That multitrack can then display
that watermark for a portion of the clip.

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
    (ellipses)
    (clip-scale (bitmap "res/fire.png"))))]

@section[#:tag "overview-rendering"]{From Programs to Videos}

Video programs can serve as standalone films or as pieces in
a larger production. In the first case, users give Video
files to a renderer. In the second case, creators pass the
prgoram to the larger project, who then renders it.

A renderer converts standalone Video programs to traditional videos.
This renderer allows creators to set various properties such
as aspect ratio, frame rate, and even output format. If no
output format is selected, Video plays a preview of the film
in a new window.

@centered[@exec{raco video -h 1920 -w 1080 --fps 48 demo.vid}]
@centered[(scale (bitmap "res/sample.png") 0.08)]

Every Video program is usable inside of other Video
programs, as well as Racket programs in general. Each module
in the language exports one @racket[vid] identifier, that
contains the described film.

@centered[(mod->pict "green.vid" "video" (clip "green"))]
@examples[#:label #f
 (eval:alts (require "demo.vid") (void))
 (eval:alts vid '((producer #hash() () color "0x00ff00ff" #f #f #f #f)))]

This exported video is a producer, which can be used in
larger projects. To streamline this process, video adds
@racket[include-video] to import video files into larger contexts.

@racketmod[video
           (clip "fire.mp4")
           (include-video "green.vid")
           (image "dragon.png")]
@centered[
 (let ([composite-pict (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                                       (scale-1080p (bitmap "res/dragon.png") 75))])
   (make-playlist-timeline
    (clip-scale (bitmap "res/dragon.png"))
    (ellipses)
    (clip-scale (filled-rectangle 50 50 #:color "green"))
    (ellipses)
    (clip-scale (bitmap "res/fire.png"))))]
