#lang scribble/sigplan

@require[scriblib/figure
         scribble/manual
         pict
         "utils.rkt"]

@title[#:tag "extensions"]{Extending Video with Graphical Editing}

Some videos are best expressed with a graphical NLVE. Video
comes with a prototype NLVE (shown in
@figure-ref["video-gui"]). Unlike other NLVEs with
scriptable APIs, the NLVE is actually part of the language.
Creators place an NLVE directly in their code, and can
include further code snippets inside of the NLVE.

@figure["video-gui" "Graphical Editor for Video Programs"]{
 @(scale (bitmap "res/video-gui.png") 0.3)}

Consider the case where a hardware failure prevents a
speaker's screen from being captured during a talk.
Fortunately, the speaker may have their own slide deck as a
PDF document. While the recording can be rebuilt by using
the slide deck, anyone that does this will have to determine
the duration of each slide. A cleaner way is embed a NLVE in
the code to construct the slide feed, and use this result in
the program. Programs that use this method inevitably result
in a long list of magic numbers. Instead, authors place a
NLVE in their code to represent the slides. Doing so gives
them a visual representation of the slides. Here is an
example of video using magic numbers (left), and the
prototype NLVE (right):

@(split-minipage
  @codeblock|{
             #lang video
             @playlist-map[(λ (slide time)
                             @image[slide
                              #:length (* 48 time)])
                           @build-path["stephen"]
                           @list[10 15 16 16 21 30 30 19 3
                                 10  50 15 33 250 42 20 65
                                 13 9 25 37 25 13 30 39 45]]}|
  (centered (scale (bitmap "res/playlist-timeline.png") 0.15)))

Graphical NLVEs are themselves producers and are first class
objects in Video.
