#lang scribble/acmart

@require[scriblib/figure scriblib/footnote
         (except-in scribble/manual cite)
         pict
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "extensions"]{Teenage Mutant Ninja Turtles -ω@m{^*}}
@nested[#:style "astfootnotes"]{
 @elem[#:style "footnotetext"]{
  We failed to find the Roman numeral rendering of ω on Google.}}

@figure["video-gui" @list{Mingling graphical NLVE widgets inside of Video scripts}]{
 @(scale (bitmap "res/video-gui.png") 0.4)}

Some videos are best expressed with a graphical NLVE, and the DrRacket
extension for Video therefore comes with embedded NLVE widgets.  Unlike other
NLVEs with scriptable APIs, the NLVE widget is actually part of the
language. A developer may place an NLVE directly into a script. Best of
all, the embedded NLVE may include code snippets, which in turn can contain
yet another NLVE widget, etc. See @figure-ref["video-gui"] for a screenshot
of such nesting.

A reader may wonder why one would want such a ``turtles all
the way down'' approach to a Video IDE. Consider the case
where a hardware failure during a talk prevents the capture
of the speaker's screen. Fortunately, the speaker may have a
copy of the slide deck as a PDF document. While the captured
video can still be recreated by using the slide deck, a
decision has to be made concerning the duration of each
slide. If a plain-text Video script were to use this method,
it would inevitably contain a bunch of ``magic
numbers.'' Embedding NLVE widgets into the code explains
these ``magic numbers'' to any future reader of the code and
is thus a cleaner way to solve the problem. @Figure-ref{
 playlist-sample} illustrates this point with a simplistic
example. The module with magic numbers is on the left; the
right part of the figure shows how an embedded NLVE explains
the numbers directly. In both cases a developer must
manually determine the screen time allocated to each slide.
However, using the widget gives the author a graphical
representation of the layout, thus speeding development
time. Additionally, future authors can more easily tweak
the times by dragging and resizing clips in the widget.

Graphical NLVEs are producers and first-class
objects in Video. They can be bound to a variable, put in a
playlist, supplied to a multitrack, and so on. Integrating
the graphical and textual program in this manner allows
users to edit videos in the style that is relevant for the
task at hand. For example, the program in
@figure-ref["video-gui"] shows an implementation of the
@racket[conference-talk] function from
@secref{overview}, now implemented using NLVE widgets with embedded code
snippets.

Traditional NLVEs have several advantages over DrRacket
widgets.@note{We conjecture that these limitations are not
 fundamental to the concept of these widgets. However,
 testing that claim would require a significant amount of
 engineering compared to the rest of Video's implementation.}
For example, NLVEs such as Premier have a cursor that tracks
a notion of ``current time,'' with a preview window that
shows a low resolution but real-time sample of the video at
that position. This feature enables high-precision editing
with quick feedback. As another example, traditional NLVEs
do not have a notion of syntax error. While an NLVE project
may not be correct, it is guaranteed to describe a Video. Even with
widgets, Video programs can still have syntax errors as with
other programming languages.

Video relies on the Racket ecosystem and the DrRacket
environment to get REPL-style feedback needed for quick
video editing. As described in @secref{overview-rendering}
the @racket[preview] function shows a low-resolution (but
real time) preview of the video being edited. This function starts
the preview moments after it is called; it can additionally
be called from both the Racket REPL and any shell
environment. Developers using DrRacket can even use its
cursor as an indicator for where to preview.

The Racket ecosystem makes it possible to add NLVE support with only a
small amount of code. The editor itself plugs into the DrRacket programming
environment@cite[drscheme-jfp]. The editor itself is built on top of
Racket's graphical framework@cite[plt-tr3], which greatly facilitates such
work.  The entire editor is implemented in less than 800 lines of code. Of
this, approximately 700 lines are for the graphical editor itself, and 50
are for the integration with Video. These lines are not counted in the 2,400
lines for Video's implementation. The code implementing these NLVE widgets
is plain Racket code, and we therefore omit details of the implementation.therefore

@figure["playlist-sample" @list{Slide reconstruction using magic numbers (left) and a NLVE widget (right)}]{
@(split-minipage
  #:split-location 0.55
  #:direction "c"
  @racketmod[video
             
             (apply playlist
              (for/list ([slide (directory-list slides)]
                         [time (in-list slide-times)])
                (image slide #:length (* 48 time))))

             (code:comment "where")
             (define slide-times
               (list 75 100 105 120 50 30 30 19 3
                     10 50 15 33 250 42 20 65
                     13 9 25 37 25 13 30 39 45))]
  (vc-append
   ;(blank 1 10)
   (hc-append 5 (scale (bitmap "res/playlist-timeline.png") 0.40) (ellipses))
   (blank 1 100)))}
