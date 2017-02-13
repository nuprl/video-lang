#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         (except-in scribble/manual cite)
         (except-in pict blank)
         (except-in pict/code typeset-code code)
         racket/format
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@(current-code-font code-font)
@(get-current-code-font-size (λ () font-size))
@(current-code-line-sep code-line-sep)

@title[#:tag "case-study"]{Case Study: Conference Videos}

Video shines in situations where films are similar with
slight variations, such as conference recordings. Each talk
has a recording of the speaker, a screen capture feed, and
audio feeds for both the speaker and audience questions.
Post production composites these feed into one stream and
inserts relevant watermarks. Each talk recording plays the
feeds in lockstep, but slight artifacts in recording vary
from talk to talk.

The high amount of repeatability makes Video an ideal
language for processing conference recordings. Every video
draws from a single library which automatically converts the
raw feeds into a processed video. Additionally, Video's
terse syntax enables the post-producer to focus on the
unique aspects of each feed, such as combining multiple
files together, noise reduction, and setting cut points.

The remainder of this section provides a case study for
Video: editing conference videos for
RacketCon.@note[racketcon-url] We show the common utility
functions that every conference talk shares. Next, we show
how creators use these functions to streamline their editing
process.

@section{Compositing Videos}

Playing the slide capture and speaker recording in lockstep
is the first step in editing conference videos. The
@racket[make-speaker-slide-composite] function composites
the slide feed and the speaker feed into one producer. As
before, @racket[where] is lifted to the top of the function
to define @racket[background]. Both @racket[speaker] and
@racket[slides] are placed on top of this background using
@racket[composite-transition]. Finally, the editor adds a
conference logo to the bottom left of the screen.

@(split-minipage
  #:split-location 0.575
  @racketblock[(code:comment "Combine the video feed for a conference recording")
               (code:comment "Producer Producer -> Producer")
               (define (make-speaker-slide-composite speaker slides)
                 @multitrack[speaker slides logo background
                             #:transitions
                             @list[@composite-transition[0 0 3/10 1
                                    #:top speaker
                                    #:bottom background]
                                   @composite-transition[0 1/2 3/10 1
                                    #:top logo
                                    #:bottom background]
                                   @composite-transition[1/3 0 2/3 1
                                    #:top slides
                                    #:bottom background]]]
                       @(define background
                              @blank[@properties-ref[speaker
                                                     'length]]))

              (define logo @image["logo.jpg"])]
  (centered
   (scale-1080p (bitmap "rconframes/stephen50.jpg") 225)))

Editors next put a splash screen at the start and end of the
video. Here, @racket[make-talk-video] uses a playlist to
place the main talk video between the two logos.
Additionally, two @racket[fade-transitions] smooths the
transitions between the logos and the recording.

@(minipage
 @racketblock[(code:comment "Add conference logos to the front and end of a video.")
              (code:comment "Producer -> Producer")
              (define (make-talk-video main-talk)
                (playlist begin-clip
                          @fade-transition[200]
                          main-talk
                          @fade-transition[200]
                          end-clip)
                (define begin-clip @image[logo #:length 500])
                (define end-clip @image[logo #:length 500]))]
 (centered
  rcon-timeline))

Finally, @racket[attach-audio] attaches a higher quality
recording of presenter's audio to the video. Only one feed
is provided because audience questions are not recorded to a
separate track. An @racket[offset] parameter allows the
function to start playing audio from the middle of the audio
file. Finally, the function adds an enveloping effect to the
start and end of the audio. This effect is analogous to the
fade transition and enables the viewer to feel a natural
start and end to the recording. Audio attachment happens
after the conference logos so that audio plays in the
background, even when a video is a splash screen.

@(split-minipage
  #:split-location 0.6
  @racketblock[(code:comment "Add higher quality speaker recording")
               (code:comment "Producer Number -> Producer")
               @(define (attach-audio video audio offset)
                  @multitrack[video cleaned-audio
                              #:length (property-ref video 'length)]
                  (define cleaned-audio
                     (attach-filters
                      audio
                      (list @project-filter[#:in offset]
                            (envelope-filter
                             50 #:direction 'in)
                            (envelope-filter
                             50 #:direction 'out)))))]
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

@section{Putting it all Together}

Creators edit a conference video by composing the previous
three functions together. While editors could do this by
hand, @racket[make-conference-talk] provides a clean
interface to generate processed recordings. This function
takes parameters for the speaker video feed, the slide
capture feed, the audio feed, and an offset for audio. It
combines these feed by composing the previous three functions
together.

@racketblock[(define (make-conference-talk speaker slides audio offset)
               @attach-audio[video audio offset]
               @(define* _ @make-speaker-slides-composite[speaker slides])
               @(define* _ @make-talk-video[_])
               @(define video @make-talk-video[_]))]

This function, which composes the previous three together,
uses @racket[where*]. Like @racket[where], all
@racket[where*] forms get lifted to the top of their scope.
The difference is that @racket[where*] can bind the same
identifier multiple times. The semantics for @racket[where*]
are otherwise identical to Racket's @racket[let*]. Any
@racket[where] binding can reference a @racket[where*]
binding. However, no @racket[where*] binding can reference a
@racket[where] binding. Furthermore, the ordering of
@racket[where*] is significant. A @racket[where*] binding
can only access previous @racket[where*] bindings.
Additionally, each @racket[where*] binding shadows any
previous @racket[where*] binding of the same name. As an
example, the following two programs have equivalent
bindings.

@(split-minipage
  @racketmod[video
             a
             (define* _ 24)
             (define* _ (+ _ 5))
             (define a (/ _ 2))
             (define* _ 84)
             (define* b (* _ 2))]
  @racketmod[racket
             (let* ([_ 24]
                    [_ (+ _ 5)])
               (define a (/ _ 2))
               (let* ([_ 84])
                 (define b (* _ 2))
                 a))])

Using @racket[make-conference-talk] makes creating the video
straightforward. @Figure-ref["video-example"] shows an
example Video program for a single RacketCon talk. The
entire program is 7 lines of code to describe a 20 minute
talk. Recording devices split video and audio files into
smaller chunks, which editors combine later using playlists.
Additionally, the start and end times for the feeds are set
with the @racket[#:start] and @racket[#:end] keywords. The
@tt["conference-lib.vid"] library stores common utilities
for putting together RacketCon videos. In particular, it
defines @racket[make-conference-talk] as shown above.

Running @exec{raco video} renders this file to an actual
video. This command renders it to an @tt{MP4} file, rendered
at 1080p and 48 frames a second. Once finished, the resulting video is
@tt["chang.mp4"], stored in the same directory as the source.

@exact{\vspace{0.5cm}}
@(nested @exec{raco video --width 1920 --height 1080 --fps 48 --mp4 chang.vid})
@exact{\vspace{0.5cm}}

Using a common utilities file reduces the implementation
costs of every conference video. To demonstrate this,
@figure-ref["video-lengths"] shows the lines of code used to
implement the RacketCon 2016 videos. Every video
implementation is less than 10 lines of code. The shortest
talk ``The Making of `Beautiful Racket'@exact{{}}'' is only
5 lines because the present had no slides. Additionally,
hardware failure caused a screen recording failure for the
first five talks. A slide deck appears in each video to
compensate for the loss, but increases the program's size.

@figure-here["video-lengths" "Length of RacketCon Video Programs"]{
@exact|{\begin{tabular}{@{}llrr@{}}\toprule
 Talk Title                           & Presenter                     & Run Time & Lines of Code \\
 \midrule
 Synthesis and Verification for All   & Emina Torlak                  & 1:00:07  & 9 \\
 Languages in an Afternoon            & Alexis King                   & 18:30    & 8 \\
 Generative Art with Racket           & Rodrigo Setti                 & 12:53    & 10 \\
 Racket is my Mjolnir                 & Geoffrey Knauth               & 28:26    & 8 \\
 Functional Lighting                  & Bruce Steinberg               & 14:14    & 10 \\
 Contracts for Security               & Scott Moore                   & 20:37    & 7 \\
 Type Systems as Macros               & Stephen Chang and Alex Knauth & 22:06    & 7 \\
 The Making of ``Beautiful Racket''   & Matthew Butterick             & 18:38    & 5 \\
 Population game simulation in Racket & Linh Chi Nguyen               & 18:07    & 7 \\
 Spelunking through JPEG with Racket  & Andy Wingo                    & 23:20    & 7 \\
 R-r-r-r-REMIX                        & Jay McCarthy                  & 20:25    & 8 \\
 Racket Does Dijkstra                 & Byron Davies                  & 22:25    & 7 \\
 Language Integrated Nitpicking       & Jack Firth                    & 17:23    & 8 \\
 \bottomrule
 \end{tabular}}|}
