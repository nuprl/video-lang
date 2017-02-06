#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Being Iavor Diatchki}

Imagine yourself being Iavor Diatchki. He is the friendly
guy who tapes all the wonderful ICFP presentations and edits
them into digestible YouTube video clips. Or imagine
yourself being Leif Andersen, who plays Diatchki's role for
RacketCon. When they create the video clips, they combine a
feed of the presenter with the presenter's screen, the sound
feed for the speaker, and yet another one for audience
questions. Additionally, Diatchki and Andersen must add a
start and end sequence to each video plus various watermarks
throughout. Composing these feeds means overlaying
them in such a manner that the result appears to be a
holistic production from the perspective of the viewer.

Once one video is put together, the same process must be
repeated for the next conference talk and the next and so
on. Worse still, even though some editing steps involve
creativity, the process becomes so monotonous that it
reduces the creative spirit for when it is truly needed. The
problem calls out for a computational solution, and the ICFP
community is well equipped to create one.

@; embedded in Racket@cite[plt-tr1]

This paper presents @emph{Video} and its IDE. Video is an
embedded domain-specific language (DSL) for video clip
production. Video is a functional language because a
functional-declarative approach mirrors what the editors of
videos actually want. Its programs automate the repetitive
aspects of the production process. Automation then helps
producers focus on the creative aspects of their video
production.

Abstractly speaking, Video makes a new and unique contribution to the thread
of ideas on domain-specific functional languages. It is an example of a
domain-specific language of descriptions (see
@secref{rationale}). Technically, Video programs describe run-time effects on
video clips, which the renderer eventually executes.

@; DrRacket@cite[plt-tr2] development environment to mix

Since most ICFP readers are probably unfamiliar with the process of editing
videos, the paper starts with a survey of the state of the art of video
editing and computational solutions (@secref["background"]). The survey
suggests a natural design blueprint for a functional video processing
language (@secref["overview"]). One form of validation is a Video program
that edits conference videos (@secref["case-study"]). It turns out that
creating the language, writing the program, and editing videos for
RacketCon 2016 takes less time than editing them manually.  Video is
implemented in Racket, which greatly facilitates the creation of
production-level DSLs (@secref["implementation"]).  Racket
also makes it easy to equip Video with a graphical IDE; that is, we can
adapt DrRacket to Video with a few lines of code so that it supports the
creation of programs as texts as well as short, embedded video clips
(@secref["extensions"]).
