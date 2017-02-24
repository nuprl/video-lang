#lang scribble/sigplan

@require[scriblib/figure
         (except-in scribble/manual cite)
         pict
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "extensions"]{Teenage Mutant Ninja Turtles -ω} @margin-note*{We
failed to find the Roman numeral rendering of ω on Google.}

@figure["video-gui" "Graphical Editor for Video Programs"]{
 @(scale (bitmap "res/video-gui.png") 0.4)}

Some videos are best expressed with a graphical NLVE, and
Video therefore comes with a prototype one. See
@figure-ref["video-gui"] for a screenshot of the editor.
Unlike other NLVEs with scriptable APIs, the NLVE widget is
actually part of the language. Developers place an NLVE
directly in their code. Best of all, they can include
further code snippets inside of such a NLVE widget, which in
tern can contain yet another NLVE widget and so on.

The Racket eco-system makes it possible to add NLVE support
with only a small amount of code. The editor itself plugs
into the DrRacket programming
environment@cite[drscheme-jfp]. Furthermore, while this
environment is used to write the modules, any Racket program
can interact with them, including ones that do not come with
the environment. We show the basic use and functionality for
these editors, as well as the effort involved in implementing 
them.

Consider the case where a hardware failure during a talk
prevents the capture of the speaker's screen. Fortunately,
the speaker may have a copy the slide deck as a PDF
document. While the captured video can still be recreated by
using the slide deck, a decision will have to be made
concerning the duration of each slide. Programs that use
this method inevitably result in a long list of magic
numbers. A cleaner way is to embed a NLVE in the code and to
construct the slide feed there manually. Doing so gives them
a visual representation of the slides and explains the image
numbering. @Figure-ref{playlist-sample} shows example of
such a recreation using magic numbers (left), and the
prototype NLVE widget (right). In the first case, half of
the program is just numbers depicting slide lengths, while
the other half is file management. The NLVE in the second
case, however, handles both of these tasks, leaving the
program as a simple timeline with a graphical representation
of slide lengths.

Graphical NLVEs are producers and are thus first-class
objects in Video. They can be bound to a variable, put in a
playlist, supplied to a multitrack, and so on. Integrating
the graphical and textual program in this manner allows
users to edit videos in the style that is relavent for the
task at hand. For example, the program in
@figure-ref["video-gui"] shows an implementation of the
@racket[conference-talk] function (used in
@secref["overview"]), but now implemented using NLVE widges.

Racket's graphical framework@cite[plt-tr3] facilitates the
development of this prototype. The entire editor is
implemented in less than 800 lines of code. Of this, 697
lines are for the graphical editor itself, and 35 are for
integration in Video programs. These lines are not counted
in the 2,400 lines for Video's implementation. The code
implementing these NLVE widgets is plain Racket code and is
not interesting from a DSL creation perspective. We
therefore do not provide details on its implementation but
instead point to the manual for DrRacket@cite[plt-tr2].

@figure["playlist-sample" "Slide reconstruction using magic numbers (left) and a NLVE widget (right)"]{
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
  (centered (hc-append 5 (scale (bitmap "res/playlist-timeline.png") 0.40) (ellipses))))}
