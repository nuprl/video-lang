#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Being Iavor Diatchki}

Imagine the process of putting together conference videos.
Each video has a feed of the presenter, as well as the
presenter's screen. Additionally, conferences talks have a
separate sound feed for the speaker, and audience questions.
Editing a talk video involves @emph{compositing} these
feeds, or overlaying them for the person watching the video.
Additionally, each video contains an start/end sequence and
various watermarks throughout the video.

Once one video is put together, the same process must then
be repeated for every conference talk. Worse still, while
there is creativity in putting together a single video, the
process gets monotonous and trivially repeatable with each
talk. When faced with a repetitive task like this one, a
computer scientist reaches for a way to script it.

@emph{Video}, a domain-specific language embedded in
Racket@cite[plt-tr1], automates the repetitive aspects of
video production. It allows producers to focus on the actual
content of their videos, while also providing a full
programming language when appropriate. Finally, as a certain
amount of video editing benefits from having a graphical
user interface, we leverage the DrRacket@cite[plt-tr2]
development environment to mix graphical and textual video
editing.

This paper describes the usage, design, and implementation
of Video. First, @secref["background"] gives the current
state of APIs and DSLs for video editing and other
non-traditional programming tasks. We introduce the design
of Video in @secref["overview"]. @Secref["case-study"] shows
a practical use case for Video, editing conference videos.
Next, @secref["implementation"] discusses how Video is
implemented. In @secref["extensions"] we discuss graphical
extensions to video to aid in editing, and how it fits into
DrRacket's work flow. We compare Video to related work in
@secref["related"] and finally conclude in
@secref["conclusion"].
