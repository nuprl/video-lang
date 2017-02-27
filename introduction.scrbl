#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Being Iavor Diatchki}

Imagine being Iavor Diatchki. He is the friendly guy who records all the
wonderful ICFP presentations, edits them into digestible video clips, and
finally creates a YouTube channel for the whole conference.  When he
creates the video clips, he combines a feed of the presenter with the
presenter's screen, the sound feed for the speaker, and yet another one for
audience questions. Additionally, Diatchki must add a start and end
sequence to each video plus various watermarks throughout. Composing these
feeds means overlaying them in such a manner that the result appears to be
a holistic production from the perspective of the viewer.

Once one video is put together, the same process must be
repeated for the next conference talk and the next and so
on. Worse, even though some editing steps involve
creativity, the process becomes so monotonous that it
reduces the creative spirit for when it is truly needed. 

@;{The problem calls out for a computational solution, and the ICFP
community is well equipped to create one.}

@; embedded in Racket@cite[plt-tr1]

The problem cries out for a declarative language, especially
because the state of the art for video editors suggests (see
@secref["background"]) that professionals in this domain already think
``functionally.'' To wit, professionals speak of ``non-linear video
editing'' (NLVE) to highlight the idea that the process is
non-destructive. Technically, the editing process separates a descriptive
phase---what the eventual video is supposed to look like, given existing
tracks---from the rendering phase---which actually creates the video clip
from these descriptions.

This pearl presents @emph{Video} (@secref["overview"]), a scripting
language for video production.  Video turns video editing upside
down. Instead of sitting for hours on end in front of some NLVE GUI, a
professional can now spend a few minutes in front of an IDE to create a
Video script and, voilà, a video clip pops out. A Video script is just a
sequence of expressions, which describe fragments of a video clip, and
definitions, which introduce constants for, and functions on, video
clips. Running such a script turns this description into suitable
``assembly code'' for a video renderer.

Speaking of an IDE, everyone knows that in this day and age a programming
language comes with a whole suite of gadgets, that is, the programming
environment @emph{around} the language. We therefore throw in an IDE
(@secref{extensions}) and a dependent type system (@secref{types}), not
only because these might be useful for producing video channels, but
because a real functional pearl deserves this much attention. Thus an
Agda-trained programmer may add types to Video modules. And better still,
Video code may include an NLVE widget, which may of course come with
embedded Video code, which may contain another NLVE widget, ... Did we
mention turtles yet?@note{See
@url{en.wikipedia.org/wiki/Turtles_all_the_way_down}, last visited Feb 20,
2017.}

Abstractly speaking, Video once again demonstrates the power of the Racket
doctrine (@secref{rationale}).  Racket hosts Video as an embedded
domain-specific language.  Implementing (@secref["implementation"]) this
language in Racket takes only a small effort because of Racket's powerful
language production tools.  Indeed, adding an IDE and a type system is
also a matter of a few hours of thinking time and coding. In this spirit, the
pearl really uses Video to illustrate the ease with which Racket developers
create @emph{real} languages as real-world problem-solving tools. It thus
points out a key difference between the construction of embedded DSLs in
boring functional languages, such as OCaml and Haskell, and the one
true-blue kid on the block, Racket.
