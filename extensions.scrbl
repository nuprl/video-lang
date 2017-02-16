#lang scribble/sigplan

@require[scriblib/figure
         (except-in scribble/manual cite)
         pict
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "extensions"]{Teenage Mutant Ninja Turtles ω} @margin-note*{We
could not figure out how to express ω as a Roman numeral, at which point we
started wondering what the NFL will do with the Super Bowl when it reaches
this age.}

Some videos are best expressed with a graphical NLVE. Video
comes with a prototype NLVE (shown in
@figure-ref["video-gui"]). Unlike other NLVEs with
scriptable APIs, the NLVE is actually part of the language.
Creators place an NLVE directly in their code, and can
include further code snippets inside of the NLVE.

Just like the rest of the Video language, the Racket
eco-system makes adding NLVE support possible with only a
small amount of code. The editor itself plugs into the
DrRacket programming environment@cite[plt-tr2]. However,
modules not written with DrRacket can also interact with
modules that do use the editor. The remainder of this
section discusses the use of these editors, and how they
interact with Video programs.

@figure["video-gui" "Graphical Editor for Video Programs"]{
 @(scale (bitmap "res/video-gui.png") 0.3)}

Consider the case where a hardware failure prevents a
speaker's screen from being captured during a talk.
Fortunately, the speaker may have their own slide deck as a
PDF document. While the recording can be rebuilt by using
the slide deck, anyone that does this will have to determine
the duration of each slide. A cleaner way is to embed a NLVE in
the code to construct the slide feed, and use this result in
the program. Programs that use this method inevitably result
in a long list of magic numbers. Instead, authors place a
NLVE in their code to represent the slides. Doing so gives
them a visual representation of the slides. Here is an
example of video using magic numbers (left), and the
prototype NLVE (right):

@(split-minipage
  @racketmod[video
             (apply playlist
                    (for/list ([slide (directory-list slides)]
                               [time (in-list slide-times)])
                      (image slide #:length (* 48 time))))
             (define slide-times
               (list 10 15 16 16 21 30 30 19 3
                     10  50 15 33 250 42 20 65
                     13 9 25 37 25 13 30 39 45))]
  (centered (scale (bitmap "res/playlist-timeline.png") 0.15)))

Graphical NLVEs are themselves producers and are first class
objects in Video. They can be bound to a variable and
further manipulated. Integrating the graphical and textual
program in this manner allows users to edit videos in the
style that is relavent for the task at hand. For example,
the following program uses @racket[make-conference-talk]
from @figure-ref["video-example"], but uses a NLVE to build
the slides:

@(minipage
  (centered
   (hc-append 10
              (scale (bitmap "res/video+nlve.png") 0.4)
              (vc-append 50 (blank) (ellipses #:offset 5)))))

In addition to placing graphical NLVEs in a program, code
can also appear in the NLVEs. Going back to the conference
example, a video editor could actually implement
@racket[make-conference-talk] using primarily graphical
video clips:

@(minipage
  (centered
   (scale-to-fit (bitmap "res/nlve+code.png") 500 400 #:mode 'distort)))

These graphical editors are strictly less powerful than both
writing the code out directly and using a traditional NLVE.
Rather, they provide authors a visual representation of the
movie they are creating, while also allowing code to be
integrated into the editor.

Racket's graphical framework@cite[plt-tr3] facilitates the
development of this prototype. The entire editor is
implemented in less than 800 lines of code. Of this, 697
lines are for the graphical editor itself, and 35 are for
integration in Video programs.
