#lang scribble/sigplan

@require[scriblib/figure
         pict]

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
PDF document. While a programmer can use this slide deck to
reconstruct the missing recording, 
