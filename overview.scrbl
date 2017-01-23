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
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)
@(define blank-clip (clip-scale (blank 1)))

@title[#:tag "overview"]{The Design of Video}

The way filmmakers create videos is at the heart of the
Video language. They need tools that allow them to describe
the video while also automating appropriate work. An
embedded DSL solves this task; it provides creators with a
declarative language designed for composing videos and has
the full power of its host language.

@Figure-ref["video-example"] shows a six frame Video program
that uses several constructs in the language---producers,
playlists, multitracks, properties, filters, and
transitions.@note{Images from this example are taken from
 Sintel. (Note, should I cite this directly?:
 @url["http://dl.acm.org/citation.cfm?id=2019066"])} Video
is terse, this six frame program can also reasonably produce
6000 frames by changing a few constants. The first line of
the program, @code{ #lang video}, is required at the top of
every video program. The rest of the program is an
interleaving of expressions and definitions. Video combines
each expression to produce the final video. Definitions are
lifted, allowing creators to place them at the bottom of the
file. Additionally, Video uses similar syntax to Scribble,
an embedded DSL for describing
documents@cite[scribble-icfp]. This syntax allows authors to
focus on the movies they are creating rather than syntax of
the language.

We discuss the language constructs used in program
throughout the section. First, we describe basic producers.
Then, we discuss the basics of how to combine producers
using playlists and multitracks. To make compelling
examples, we simultaneously introduce transitions, filters,
and properties. Finally, we describe the interface authors
use to render their programs into traditional video files.

@figure["video-example" "A Sample Video Program"]{
 @codeblock|{
#lang video

@color["green" #:length 1]

@multitrack[
   @clip["fire.png" #:length (/ (property-ref blue-clip 'length) 8)]
   @composite-transition[0 0 1/2 1/2]
   blue-clip
 #:length 5]

@swipe-transition[#:direction 'up #:length 2]

@image["dragon" #:length 3]

@where[blue-clip <- @color["blue" #:length 8]]}|
 @exact{\vspace{0.3cm}} 
  @(centered
  (scale
   (make-playlist-timeline
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (lt-superimpose (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                    (scale (clip-scale (bitmap "res/fire.png")) 1/2))
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (vc-append
     (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                 0 0 0 (* (pict-height blank-clip) -1/3))
     (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 (* (pict-height blank-clip) -2/3) 0 0))
    (vc-append
     (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                 0 0 0 (* (pict-height blank-clip) -2/3))
     (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 (* (pict-height blank-clip) -1/3) 0 0))
    (clip-scale (bitmap "res/dragon.png")))
   1.6))}

@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the basic building block for video
programs. A producer is any data that can be coerced into
video---audio clips, video clips, pictures, and even some
internal Racket data structures. Video programs combine all
of the producers and a render converts them into a
traditional video file. The power of Video lies in the fact
that combinations of producers are themselves producers, and
can be further combined into yet more complex producers
still.

The simplest type of producer is one of Racket's internal
data structures for drawing pictures@cite[slideshow-jfp].
Colors, for example, are constructed with the @racket[color]
function. It creates a colored producer with an unspecified
length.

@(split-minipage
  @codeblock|{
   @color["green"]}|
  (centered
   (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 150)))

A length can be given to the color with the
@racket[#:length] keyword. This keyword takes in the number
of frames the producer should contain. If this keyword is
not present, then @racket[color] will generate a producer
long enough to fill any media it is combined with.

@(split-minipage
  @codeblock|{
   @color["blue" #:length 2]}|
  (centered
   (make-playlist-timeline
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue")))))

Another form, @racket[clip], creates a producer from a
traditional video or audio file. Either the clip is imported
in its entirely, or the @racket[#:start] and @racket[#:end]
keywords project a slice of the clip into the program. If
used, these keywords specify the initial and final frames
that are are included. The @racket[#:length] keyword can be
used here when only the length of the clip is relevant,
otherwise the length will be the difference between the
start and end frame number.

@(split-minipage
  @codeblock|{@clip["fire.mp4" #:start 100 #:end 103]}|
  (centered
   (make-playlist-timeline
    (clip-scale (bitmap "res/fire.png"))
    (clip-scale (bitmap "res/fire.png"))
    (clip-scale (bitmap "res/fire.png")))))

Pictures have their own constructor: @racket[image]. Unlike
clips, images do not have an implicit start and end time.
Rather, like colors, they can fill as much or little time as
needed. If the pictures display time is important,
@racket[#:length] constructs a producer with a specified
length.

@(split-minipage
  @codeblock|{@image["dragon.png" #:length 1]}|
  (centered (scale-1080p (bitmap "res/dragon.png") 150)))

The final important form is @racket[blank]. Like
@racket[image], it takes a @racket[#:length] parameter. A
blank is equivalent to inserting a completely transparent
color. While blank is not useful on its own, it is useful for creating playlists with an offset in their start time.

@(split-minipage
  @codeblock|{@blank[2]}|
  (centered
   (make-playlist-timeline
    (clip-scale (rectangle 50 50))
    (clip-scale (rectangle 50 50)))))

@section{Playlists}

A video is merely a composition of many clips. Video
provides two main ways for combining videos:
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler video
composite form. They are syntactically similar to lists.
Any producer can be put in a playlist. Each clip in the playlist
will play in succession.

@;{
Both data types additionally rely on
@emph{transitions} and @emph{filters}. Transitions inform
playlists and multitracks how multiple clips merge, such as
fading or overlays. Filters, unlike transitions, change the
behavior of a single producer---change the color, change
playback speed, etc. Finally, producer properties allow for
programs with dependent producers.
}

@(split-minipage
  @codeblock|{@playlist[@image["dragon.png"]
                        @clip["fire.mp4"]]}|
  (centered (make-playlist-timeline
             (clip-scale (bitmap "res/dragon.png"))
             (clip-scale (bitmap "res/fire.png"))
             (ellipses))))

Creators insert blanks into their playlist to create a
placeholder frame that does not have any content. This is
used if they playlist should have an offset at the start, or
a gap in the middle.

@(split-minipage
  @codeblock|{@playlist[@image["dragon.png"]
                        @blank[1]
                        @clip["fire.mp4"]]}|
  (centered (make-playlist-timeline
             (clip-scale (rectangle 50 50))
             (clip-scale (rectangle 50 50))
             (clip-scale (bitmap "res/dragon.png"))
             (clip-scale (bitmap "res/fire.png"))
             (ellipses))))

Standard list operations also work with playlists. For
example, @racket[playlist-append] creates a new playlist comprised of
the playlists given to it.

@(split-minipage
  @codeblock|{@playlist-append[dragon-clips color-clips]
              @where[dragons <- @playlist[@image["dragon.mp4"]
                                          @clip["fire.mp4"]]]
              @where[colors <- @playlist[@color["red"]
                                         @color["blue"]]]}|
  (centered
   (make-playlist-timeline
    (clip-scale (bitmap "res/dragon.png"))
    (clip-scale (bitmap "res/fire.png"))
    (ellipses)
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "red"))
    (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue")))))

@section{Transitions}
Jumping from one producer to another in a playlist
is jarring. Movies frequently reduce this effect with
@emph{transitions}---fading, swiping, etc. These transitions merge
two adjacent clips in a playlist.

Creators place transitions inside playlists directly. The
transition mixes the clip above and below it in the list.

@(split-minipage
  @codeblock|{@playlist[@image["dragon.png" #:length 3]
                        @swipe-transition[#:direction 'bottom
                                          #:duration 2]
                        @clip["fire.mp4" #:length 3]]}|
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      (clip-scale (bitmap "res/dragon.png"))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -1/3))
       (inset/clip (clip-scale (bitmap "res/fire.png")) 0 (* (pict-height size) -2/3) 0 0))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -2/3))
       (inset/clip (clip-scale (bitmap "res/fire.png")) 0 (* (pict-height size) -1/3) 0 0))
      (clip-scale (bitmap "res/fire.png"))))))

Every transition in a playlist actually shortens the length
of the playlist. This effect stems from the fac that
transitions produce one clip for every two clips they
consume. For example, the playlist above has six frames without its transition, rather than four.

@(split-minipage
  #:split-location 0.4
  @codeblock|{@playlist[
               @image["dragon.png" #:length 3]
               @clip["fire.mp4" #:length 3]]}|
  (centered
    (make-playlist-timeline
     (clip-scale (bitmap "res/dragon.png"))
     (clip-scale (bitmap "res/dragon.png"))
     (clip-scale (bitmap "res/dragon.png"))
     (clip-scale (bitmap "res/fire.png"))
     (clip-scale (bitmap "res/fire.png"))
     (clip-scale (bitmap "res/fire.png")))))

Playlists can also have multiple transitions. Transitions,
however, are associative operations. Therefore multiple
transitions can be placed in a single playlist without any
unexpected effects.

@(split-minipage
  @codeblock|{@playlist[@image["dragon.png" #:length 2]
                        @swipe-transition[#:direction 'bottom
                                          #:duration 1]
                        @color["blue" #:length 2]
                        @swipe-transition[#:direction 'top
                                          #:duration 1]
                        @clip["fire.png" #:in 0 #:out 2]]}|
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      (clip-scale (bitmap "res/dragon.png"))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -1/2))
       (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                   0 (* (pict-height size) -1/2) 0 0))
      (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/fire.png")) 0 0 0 (* (pict-height size) -1/2))
       (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                   0 (* (pict-height size) -1/2) 0 0))
      (clip-scale (bitmap "res/fire.png"))))))

@section{Multitracks}

Unlike playlists, multitracks play producers in
parallel. Like image layers, the renderer can only display
one track. Developers use transitions to composite the
tracks together.

Multiracks are syntactically similar to playlist. The
@racket[multitrack] form accepts a list of producers and
creates a new multitrack producer. Like playlists,
transitions are place directly in the list to combine
tracks.

@(split-minipage
  @codeblock|{@multitrack[
               @clip["fire.mpg"]
               @composite-transition[0 0 1/2 1/2]
               @image["dragon.png"]]}|
  (centered
   (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                   (scale-1080p (bitmap "res/dragon.png") 75))))

Unlike playlists, transitions in multitracks are not associative. Multricks
interpret transitions in left to right order. Videos that
require a different evaluation order can embed a multitrack
inside of a multitrack. This is possible because, like
playlists, multitracks are themselves producers.

@(split-minipage
  @codeblock|{@multitrack[
               @clip["fire.png"]
               @composite-transition[0 0 1/2 1/2]
               @multitrack[
                @image["dragon.png"]
                @composite-transition[0 1/2 1/2 1/2]
                @color["green"]]]}|
  (centered
   (lb-superimpose
    (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                    (scale-1080p (bitmap "res/dragon.png") 75))
    (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))

A second alternative is to apply multiple transitions to the
same track. The @racket[#:transitions] keyword composites a
top and bottom producer in the multitrack.

@(split-minipage
  @codeblock|{@multitrack[dragon color bg
                          #:transitions
                          @list[
                           @composite-transition[0 0 1/2 1/2
                                                 #:top dragon
                                                 #:bottom bg]
                           @composite-transition[1/2 0 1/2 1/2
                                                 #:top red-color
                                                 #:bottom bg]
                           @composite-transition[0 1/2 1/2 1/2
                                                 #:top green-color
                                                 #:bottom bg]]]
              @where[bg <- @clip["fire.png"]]
              @where[dragon <- @image["dragon.png"]]
              @where[green-color <- @color["green"]]
              @where[red-color <- @color["red"]]}|
  (centered
   (lb-superimpose
    (rt-superimpose
     (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                     (scale-1080p (bitmap "res/dragon.png") 75))
     (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "red") 75))
    (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))

Racket's @racket[eq] function determines producer
equivalence. Multitracks are able to use this pattern
because producers are functional objects.

The @racket[#:transitions] keyword also applies to
playlists. While not strictly needed for playlists, it
allows authors to create functions that insert transitions
into playlists without having to bother with list operations directly.

@(split-minipage
  @codeblock|{@swiping-playlist[dragon green-color]
              @where[dragon <- @image["dragon.png"]]
              @where[green-color <- @color["green"]]
              @where[swiping-playlist <-
               (λ (a b)
                 @playlist[a b
                  #:transitions
                    @list[@swipe-transition[#:direction 'bottom
                                            #:duration 2
                                            #:first a
                                            #:second b]]])]}|
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      (clip-scale (bitmap "res/dragon.png"))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -1/3))
       (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                   0 (* (pict-height size) -2/3) 0 0))
      (vc-append
       (inset/clip (clip-scale (bitmap "res/dragon.png")) 0 0 0 (* (pict-height size) -2/3))
       (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                   0 (* (pict-height size) -1/3) 0 0))
      (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))))))
  

@section{Filters}

Filters are similar to transitions that modify the behavior
of only a single producer. Some things filters are useful
for include removing the color from a clip or even changing
a producer's aspect ratio. Thus, filters themselves are
functions from producers to producers. For example, the
@racket[scale-filter] filter scales a producer by the given
width and height.

@(split-minipage
  @codeblock|{@scale-filter[@image["dragon.png"] 1 9]}|
  (centered
   (let ([size (scale (scale-1080p (blank 1) 150) 1 9)])
     (inset/clip
      (scale (scale-1080p (bitmap "res/dragon.png") 150) 1 9)
      0 (* (pict-height size) (+ -1/3 -1/9)) 0 (* (pict-height size) (+ -1/3 -1/9))))))

@section{Properties and Dependent Clips}

Producers use properties to store and retrieve information
about other producers. They additionally store both implicit
and explicit properties. Implicit properties are innate to a
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

@(split-minipage
  @codeblock|{@multitrack[
               fire-clip
               @composite-transition[
                0
                @if[@get-property[dragon 'bottom?]
                    1/2 0]
                 1/2 1/2]
               @image["dragon.png"]]

               @where[fire-clip <-
                 @set-property[@clip["fire"] 'bottom? #f]]}|
               
  (centered
   (lb-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                   (scale-1080p (bitmap "res/dragon.png") 75))))

Implicit properties store innate information about a clip.
Returning to the watermark example, a multitrack can get the
length of its main clip. That multitrack can then display
that watermark for a portion of the clip.

@(split-minipage
  @codeblock|{@multitrack[
               fire-clip
               @composite-transition[0 0 1/2 1/4]
               @image["dragon.png"
                      #:length (@get-property[dragon
                                              'length]
                                   . / .
                                   2)]]

              @where[fire-clip <- @clip["fire"]]}|
  (centered
   (let ([composite-pict (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                                         (scale-1080p (bitmap "res/dragon.png") 75))])
     (make-playlist-timeline
      (clip-scale composite-pict)
      (ellipses)
      (clip-scale (bitmap "res/fire.png"))
      (ellipses)))))

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

@(split-minipage
  (centered @exec{raco video -h 1920 -w 1080 --fps 48 demo.vid})
  (centered (scale (bitmap "res/sample.png") 0.08)))

Every Video program is usable inside of other Video
programs, as well as Racket programs in general. Each module
in the language exports one @racket[vid] identifier, that
contains the described film.

@(split-minipage
  (examples #:label #f
            (eval:alts (require "demo.vid") (void))
            (eval:alts vid '((producer #hash() () color "0x00ff00ff" #f #f #f #f))))
  (centered (modblock->pict "green.vid" "video" @~a|{@clip["green"]}|)))

This exported video is a producer, which can be used in
larger projects. To streamline this process, video adds
@racket[include-video] to import video files into larger contexts.

@(split-minipage
  @codeblock|{#lang video
             @clip["fire.mp4"]
             @include-video["green.vid"]
             @image["dragon.png"]}|
  (centered
   (let ([composite-pict (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                                         (scale-1080p (bitmap "res/dragon.png") 75))])
     (make-playlist-timeline
      (clip-scale (bitmap "res/dragon.png"))
      (ellipses)
      (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
      (clip-scale (bitmap "res/fire.png"))))))
