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
reduces the creative spirit for when it is truly needed. 

@;{The problem calls out for a computational solution, and the ICFP
community is well equipped to create one.}

@; embedded in Racket@cite[plt-tr1]

The problem cries out for a functional-declarative language, especially
because a survey of video editors suggests (see @secref["background"]) that
professionals in this domain already think ``functionally.'' To wit,
professionals speak of ``non-linear video editing'' (NLVE) to highlight the
idea that the process is non-destructive. Technically, the editing process
separates a descriptive phase---what the eventual video is supposed to look
like, given existing tracks---from the rendering phase---which actually
creates the video clip from these descriptions. 

This pearl presents @emph{Video} (@secref["overview"]) and its IDE
(@secref["extensions"]). As its name says, Video turns video editing upside
down. Instead of sitting for hours on end in front of some NLVE GUI, a
professional can now spend a few minutes in front of an IDE and voilà, a
video clip pops out (@secref["case-study"]). Well, a Video program is just
a sequence of expressions, which describe compositions of video clips, and
definitions, which introduce constants for and functions on videos ;
running this program rewrites this description into suitable ``assembly
code'' for a video renderer. If it is really necessary, the code may
include a video clip, which may of course come with embedded Video code,
which may contain a video clip, . . . Did we mention turtles yet?

Abstractly speaking, Video once again demonstrates the power of the Racket
doctrine. Racket hosts Video, an embedded domain-specific language.
Implementing this language in Racket takes only a few lines of code,
because of Racket's powerful language production tools
(@secref["implementation"]). These tools also allow the addition of a
useful type system in a matter of hours (@secref{types}). In this spirit,
the pearl really uses Video to illustrate the ease with which Racket
developers create languages as problem-solving tools. It thus points out a
key difference between the construction of embedded DSLs in boring
functional languages, such as OCaml and Haskell, and the one true-blue kid
on the block, Racket.
