#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Being Iavor Diatchki}

Imagine yourself being Iavor Diatchki. He is the friendly
guy who tapes all the wonderful ICFP presentations and
then edits them into digestible YouTube video clips. Or
imagine yourself being Leif Andersen, who plays Diatchki's
role for RacketCon.  

When these people create the video clips, they combine a
feed of the presenter with the presenter's screen, the
sound feed for the speaker, and yet another one for
audience questions. Composing these feeds means overlaying
them in such a manner that the result appears to be a holistic
production from the perspective of the viewer. Above and
beyond this basic task, Diatchki and Andersen must add a
start and/or end sequence to each video plus various
watermarks throughout the video.

Once one video is put together, the same process must be
repeated for the next conference talk and the next and so
on. Worse still, even though some editing steps involve
creativity, the process becomes so monotonous that it
reduces the creative spirit for when it is truly needed. The
problem calls out for a computational solution, and the ICFP
community is well equipped to create one.

@; embedded in Racket@cite[plt-tr1]

This paper presents @emph{Video} and its IDE. Video is an
embedded domain-specific language (DSL) for
post-conference video clip production.  Naturally Video is
a functional language, whose programs automate the
repetitive aspects of the production process. Automation
then helps producers focus on the creative aspects of
their video production. 

@; DrRacket@cite[plt-tr2] development environment to mix

Since ICFP readers are usually unfamiliar with the process
of editing videos, the paper's first contribution is a
survey of the state of the art of video editing and
computational solutions (@secref["background"]). The survey
and Andersen's work on RacketCon videos suggest a natural
design blueprint for a functional video processing language
(@secref["overview"]). One form of validation is a Video
program that edits conference videos; see
@secref["case-study"] for this program. It turns out that
creating the language, writing the program, and editing
videos for RacketCon 2016 takes less time than editing them
manually. Video is a member of the Racket eco-system, which
greatly facilitates the creation of production-level DSLs
(@secref["implementation"]). Since in this day and age a
programming language must come with a graphical IDE, we also
explain how to specialize DrRacket to Video with a few lines
of code so that it supports the creation of programs, both
as texts as well as short, embedded video clips as
first-class values (@secref["extensions"]). We obviously
wrap up the paper with a section on other related work and
some brief remarks on making DSLs.
