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

The preceding overview suggests that non-linear video editing distinctly
separates the description of a video clip from the rendering action on
it. Specifically, a video editor demands a description of what the final
video should look like in terms of the given pieces.  The action of
creating and rendering this video is a distinct second step. Furthermore,
experience tells us that like programs, descriptions need abstractions; for
example, a description may employ a comprehension to apply a watermark to
all images, or it may employ of one module per ICFP presentation to make up
a complete ICFP channel.

The Video language gets to the heart of the problem. Each Video program
is a complete module that intermingles descriptions of video clips and
functions. It denotes a module that exports a single item: a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. Another way is to import it into another
Video module and to incorporate the exported video clip description into a
larger one. 

@figure["video-example" "A Sample Video Program"]{
 @codeblock|{
#lang video

@color["green" #:length 1]

@multitrack[
   @image["circ.png" #:length (/ (property-ref blue-clip 'length) 8)]
   @composite-transition[0 0 3/4 3/4]
   blue-clip
 #:length 5]

@swipe-transition[#:direction 'up #:length 2]

@clip["rect.mp4" #:length 3]

@where[blue-clip <- @color["blue" #:length 8]]}|
 @exact{\vspace{0.3cm}} 
  @(centered
  (scale
   (make-playlist-timeline
    #:end #t
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
    (clip-frame
     (lt-superimpose (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                     (scale (clip-scale circ-image) 3/4)))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame
     (vc-append
      (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                  0 0 0 (* (pict-height blank-clip) -1/3))
      (inset/clip (clip-scale (first rect-clip)) 0 (* (pict-height blank-clip) -2/3) 0 0)))
    (clip-frame
     (vc-append
      (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
                  0 0 0 (* (pict-height blank-clip) -2/3))
      (inset/clip (clip-scale (second rect-clip)) 0 (* (pict-height blank-clip) -1/3) 0 0)))
    (clip-frame (third rect-clip)))
   1.6))}

@Figure-ref["video-example"] shows a six frame Video video
that uses some of its most basic constructs: producers,
playlists, multitracks, properties, filters, and
transitions.@note{Images from this example are taken from
 the movie Sintel. (Note, should I cite this directly?:
 @url["http://dl.acm.org/citation.cfm?id=2019066"])} Video is
terse; this six-frame program can also reasonably produce
6,000 frames by changing a few constants. The first line of
the program, @code["#lang video"], is required at the top of
every video program. The rest of the program is an
interleaving of expressions and definitions. Video turns the
sequence of expression to produce the final video.
Definitions are lifted, allowing creators to place them at
whatever positiong makes the program most readable.

Video uses syntax similar to Scribble, an
embedded DSL for describing documents@cite[scribble-icfp].
This syntax allows authors to focus on the movies they are
creating rather than syntax of the language.

We discuss the language constructs used in program
throughout the section. First, we describe basic producers.
Then, we discuss the basics of how to combine producers
using playlists and multitracks. To make compelling
examples, we simultaneously introduce transitions, filters,
and properties. Finally, we describe the interface authors
use to render their programs into traditional video files.

@section[#:tag "overview-simple"]{Producers}

The @emph{producer} is the most basic building block for
Video programs. A producer is any data that can be coerced
into video: audio clips, video clips, pictures, and even
some internal Racket data structures. Combinations of
producers are themselves producers, and they can be further
combined into yet more complex producers still.

The simplest type of producer, color, is one of Racket's
internal data structures for drawing
pictures@cite[slideshow-jfp]. A color producer creates a
clip of the specified color with an unspecified length:

@(split-minipage
  @codeblock|{
   @color["green"]}|
  (centered
   (make-playlist-timeline
    #:end #f
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green")))))

Alternatively, a length can be specified with the
@racket[#:length] keyword:

@(split-minipage
  @codeblock|{
   @color["blue" #:length 2]}|
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue")))))

Another producer, @racket[clip], describes a
traditional video or audio file. Either the clip is imported
in its entirely, or the @racket[#:start] and @racket[#:end]
keywords project a slice of the clip into the program. If
used, these keywords specify the initial and final frames
that are are included. The @racket[#:length] keyword can be
used here when only the length of the clip is relevant.

@(split-minipage
  @codeblock|{@clip["rect.mp4" #:start 100 #:end 103]}|
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame (fifth rect-clip))
    (clip-frame (sixth rect-clip))
    (clip-frame (seventh rect-clip)))))

Pictures have their own producer: @racket[image]. Unlike
clips, images do not have an implicit start and end time.
Rather, like colors, they can fill as much or little time as
needed. If the pictures display time is important,
@racket[#:length] constructs a producer with a specified
length:

@(split-minipage
  @codeblock|{@image["circ.png" #:length 1]}|
  (centered (make-playlist-timeline #:end #t (clip-frame circ-image))))

The final important producer is @racket[blank]. Like
@racket[image], it takes a @racket[#:length] parameter. A
blank is equivalent to inserting a completely transparent
color. While blank is not useful on its own, it is useful for creating playlists with an offset in their start time.

@(split-minipage
  @codeblock|{@blank[2]}|
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame (rectangle 50 50))
    (clip-frame (rectangle 50 50)))))

@section{Playlists}

A video is merely a composition of many clips. Video
provides two main ways for combining videos:
@emph{playlists} and @emph{multitracks}. Roughly speaking,
playlists play clips in sequence, while multitracks play
clips in parallel.

The playlist is the simpler of the two
compositing form. They are syntactically similar to lists.
Any producer can be put in a playlist. Each clip in the playlist
plays in succession:

@(split-minipage
  @codeblock|{@playlist[@image["circ.png"]
                        @clip["rect.mp4"]]}|
  (centered (make-playlist-timeline
             #:end #t
             (clip-frame circ-image)
             (clip-frame (first rect-clip))
             (ellipses))))

Insert a blanks into playlists creates a placeholder frame
that does not have any content. This is used if they
playlist should have an offset at the start or a gap in the
middle:

@(split-minipage
  @codeblock|{@playlist[@blank[2]
                        @image["circ.png"]
                        @clip["rect.mp4"]]}|
  (centered (make-playlist-timeline
             #:end #t
             (clip-frame (rectangle 50 50))
             (clip-frame (rectangle 50 50))
             (clip-frame circ-image)
             (clip-frame (first rect-clip))
             (ellipses))))

Standard list operations also work with playlists. For
example, @racket[playlist-append] creates a new playlist comprised of
the given playlists:

@(split-minipage
  @codeblock|{@playlist-append[dragon-clips color-clips]
              @where[shapes <- @playlist[@image["circ.png"]
                                         @clip["rect.mp4"]]]
              @where[colors <- @playlist[@color["red"]
                                         @color["blue"]]]}|
  (centered
   (make-playlist-timeline
    #:end #f
    (clip-frame circ-image)
    (clip-frame (first rect-clip))
    (ellipses)
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "red"))
    (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "blue")))))

This sample also introduces @racket[where] for binding. This
binding is similar to Racket's @racket[define] binding form,
but is lifted to the top of its scope. Here,
@racket[shapes] and @racket[colors] are bound to existing
playlists. Video raises these definitions to top of the
program. thus enabling to put them where they are most clear
in this program. In this case, at the bottom.

@section{Transitions}
Jumping from one producer to another in a playlist
is jarring. Movies frequently reduce this effect with
@emph{transitions}: fading, swiping, etc. These transitions merge
two adjacent clips in a playlist.

Transitions are placed directly inside playlists. The
transition mixes the two adjacent clips in the list.

@(split-minipage
  @codeblock|{@playlist[@image["circ.png" #:length 3]
                        @swipe-transition[#:direction 'bottom
                                          #:duration 2]
                        @clip["rect.mp4" #:length 3]]}|
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      #:end #t
      (clip-frame circ-image)
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/3))
        (inset/clip (clip-scale (first rect-clip)) 0 (* (pict-height size) -2/3) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -2/3))
        (inset/clip (clip-scale (second rect-clip)) 0 (* (pict-height size) -1/3) 0 0)))
      (clip-frame (third rect-clip))))))

Every transition in a playlist actually shortens the length
of the playlist, because transitions produce one clip for
every two clips they consume. For example, the playlist
above has six frames without its transition, rather than
four:

@(split-minipage
  #:split-location 0.4
  @codeblock|{@playlist[
               @image["circ.png" #:length 3]
               @clip["rect.mp4" #:length 3]]}|
  (centered
    (make-playlist-timeline
     #:end #t
     (clip-frame circ-image)
     (clip-frame circ-image)
     (clip-frame circ-image)
     (clip-frame (first rect-clip))
     (clip-frame (second rect-clip))
     (clip-frame (third rect-clip)))))

Playlists may contain multiple transitions. Videos that
contain this behavior are not ambiguous because transitions
are associative operations. Multiple transitions placed in a
single playlist described the desired clip without any
unexpected effects:

@(split-minipage
  @codeblock|{@playlist[@image["circ.png" #:length 2]
                        @swipe-transition[#:direction 'bottom
                                          #:duration 1]
                        @color["blue" #:length 2]
                        @swipe-transition[#:direction 'top
                                          #:duration 1]
                        @clip["rect.mp4" #:in 0 #:out 2]]}|
  (centered
   (let ([size (clip-scale (blank 1))])
     (make-playlist-timeline
      #:end #t
      (clip-frame circ-image)
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/2))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -1/2) 0 0)))
      (clip-frame (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale (first rect-clip)) 0 0 0 (* (pict-height size) -1/2))
        (inset/clip (clip-scale (filled-rectangle 100 100 #:draw-border? #f #:color "blue"))
                    0 (* (pict-height size) -1/2) 0 0)))
      (clip-frame (second rect-clip))))))

@section{Multitracks}

Unlike playlists, multitracks play producers in parallel.
Like with playlists transitions are used to composite the
tracks.

Multiracks are syntactically similar to playlist. The
@racket[multitrack] form accepts a list of producers and
creates a new multitrack producer. Transitions are again
placed directly in the list to combine tracks:

@(split-minipage
  @codeblock|{@multitrack[
               @clip["rect.mp4"]
               @composite-transition[0 0 1/2 1/2]
               @image["circ.png"]]}|
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lt-superimpose (scale-1080p (first rect-clip)  150)
                     (scale-1080p circ-image 75)))
    (clip-frame
     (lt-superimpose (scale-1080p (second rect-clip)  150)
                     (scale-1080p circ-image 75)))
    (ellipses))))

Transitions within multitracks are not associative. Indeed
multricks interpret transitions in left to right order.
Videos that require a different evaluation order can embed a
multitrack inside of a multitrack, because multitracks are
themselves producers. Here is an example:

@(split-minipage
  @codeblock|{@multitrack[
               @clip["rect.mp4"]
               @composite-transition[0 0 1/2 1/2]
               @multitrack[
                @image["circ.png"]
                @composite-transition[0 1/2 1/2 1/2]
                @color["green"]]]}|
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
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))))

An alternative is to apply multiple transitions to the same
track. The @racket[#:transitions] keyword takes a list of
transitions. Each transition in this list composites a two
producers, top and bottom, in the multitrack. The renderer
uses Racket's @racket[eq?] function to determine equality:

@(split-minipage
  @codeblock|{@multitrack[circ color bg
                          #:transitions
                          @list[
                           @composite-transition[0 0 1/2 1/2
                                                 #:top circ
                                                 #:bottom bg]
                           @composite-transition[1/2 0 1/2 1/2
                                                 #:top red-color
                                                 #:bottom bg]
                           @composite-transition[0 1/2 1/2 1/2
                                                 #:top green-color
                                                 #:bottom bg]]]
              @where[bg <- @clip["rect.mp4"]]
              @where[dragon <- @image["circ.png"]]
              @where[green-color <- @color["green"]]
              @where[red-color <- @color["blue"]]}|
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
      (scale-1080p (filled-rectangle 50 50 #:draw-border? #f #:color "green") 75))))))

The @racket[#:transitions] keyword also applies to
playlists. While not strictly needed for playlists, it
allows abstractions into both playlist and multitrack
functions that insert transitions into both of these data
representations without having to bother with list
operations directly. Here is an example:

@(split-minipage
  @codeblock|{@swiping-playlist[circ green-color]
              @where[circ <- @image["circ.png"]]
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
      #:end #f
      (clip-frame circ-image)
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -1/3))
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 (* (pict-height size) -2/3) 0 0)))
      (clip-frame
       (vc-append
        (inset/clip (clip-scale circ-image) 0 0 0 (* (pict-height size) -2/3))
        (inset/clip (clip-scale (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
                    0 (* (pict-height size) -1/3) 0 0)))
      (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))))))
  

@section{Filters}

Filters are similar to transitions that modify the behavior
of only a single producer. For example, filters can
remove the color from a clip or change
a producer's aspect ratio. Thus, filters themselves are
functions from producers to producers. As an example, the
@racket[scale-filter] filter scales a producer by the given
width and height, leading to a stretching effect:

@(split-minipage
  @codeblock|{@scale-filter[@image["circ.png"] 1 3]}|
  (centered
   (let ([size (scale (scale-1080p (blank 1) 150) 1 9)])
     (make-playlist-timeline
      #:end #f
      (clip-frame
       (inset/clip
        (scale (scale-1080p circ-image 150) 1 9)
        0 (* (pict-height size) (+ -1/3)) 0 (* (pict-height size) (+ -1/3))))))))

@section{Properties and Dependent Clips}

Producers use properties to store and retrieve information
about other producers. They additionally store both implicit
and explicit properties. Implicit properties are innate to
clips, such as length and dimensions. Explicit properties
are added by the program.

The properties API has two calls:
@itemlist[
 @item{@racket[(set-property #,(emph "producer") #,(emph "key") #,(emph "value"))] creates
  an explicit property. It returns a new producer with @emph{key} associated with @emph{value}.}
 @item{@racket[(get-property #,(emph "producer") #,(emph "key"))] returns
  the value associated with @emph{key}. If the property is set
  both implicitly and explicitly, the explicit property has priority.}]

Explicit properties provide a protocol for clips to
communicate information. For example, a watermark can
communicate whether it should be placed at the top or bottom of
the screen:

@(split-minipage
  @codeblock|{@multitrack[
               rect-clip
               @composite-transition[
                0
                @if[@get-property[rect-clip 'bottom?]
                    1/2 0]
                 1/2 1/2]
               @image["circ.png"]]

               @where[rect-clip <-
                 @set-property[@clip["rect.mp4"] 'bottom? #f]]}|
               
  (centered
   (make-playlist-timeline
    #:end #t
    (clip-frame
     (lb-superimpose (scale-1080p (first rect-clip) 150)
                     (scale-1080p circ-image 75)))
    (clip-frame
     (lb-superimpose (scale-1080p (second rect-clip) 150)
                     (scale-1080p circ-image 75))))))

Implicit properties store innate information about a clip.
Returning to the watermark example, a multitrack can get the
length of its main clip. That multitrack can then display
that watermark for a portion of the clip:

@(split-minipage
  #:split-location 0.45
  @codeblock|{@multitrack[
               fire-clip
               @composite-transition[0 0 1/2 1/4]
               @image["circ.jpg"
                      #:length (@get-property[dragon
                                              'length]
                                   . / .
                                   2)]]

              @where[rect-clip <- @clip["rect.mp4"]]}|
  (centered
   (let ([composite-pict (lt-superimpose (scale-1080p (first rect-clip) 150)
                                         (scale-1080p circ-image 75))]
         [composite-pict2 (lt-superimpose (scale-1080p (second rect-clip) 150)
                                          (scale-1080p circ-image 75))]
         [composite-pict3 (lt-superimpose (scale-1080p (seventh rect-clip) 150)
                                          (scale-1080p circ-image 75))])
     (make-playlist-timeline
      #:end #t
      (clip-frame composite-pict)
      (clip-frame composite-pict2)
      (ellipses)
      (clip-frame composite-pict3)
      (clip-frame (eighth rect-clip))
      (clip-frame (ninth rect-clip))
      (ellipses)))))

@section[#:tag "overview-rendering"]{From Programs to Videos}

Video programs may describe standalone films or as pieces in
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
   #:split-location 0.7
   (examples #:label #f
             (eval:alts (require "demo.vid") (void))
             (eval:alts vid '((producer #hash() () color "0x00ff00ff" #f #f #f #f))))
   @(filebox "green.vid"
             @codeblock|{#lang video
                         @clip["green"]}|))
 @(blank 20)
 @(hline 500 0)
 @(blank 20)
 @(split-minipage
   @codeblock|{#lang video
               @clip["fire.mp4"]
               @include-video["green.vid"]
               @image["dragon.png"]}|
   (centered
    (let ([composite-pict (lt-superimpose (scale-1080p (bitmap "res/fire.png") 150)
                                          (scale-1080p (bitmap "res/dragon.png") 75))])
      (make-playlist-timeline
       #:end #f
       (clip-frame (first rect-clip))
       (ellipses)
       (clip-frame (filled-rectangle 50 50 #:draw-border? #f #:color "green"))
       (clip-frame circ-image)))))}
 