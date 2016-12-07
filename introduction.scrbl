#lang scribble/sigplan

@require[scriblib/footnote
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Video Editing Process}

Video editing, while creative, is often a repetitive task.
For example, editing a conference talk frequently involves
compositing a camera feed with a screen capture feed and one
for audio capture. Compositing these feeds takes several
steps, each video must be shrunk and placed on the
appropriate part of the screen, as well as being
synchronized. Each step only takes a few mouse clicks, but
in aggregate they become a large task. This entire process
then needs to be repeated for every video, significantly
increasing the work load. On top of all of this, each video
may also include the conference logo at the start and end of
the video, and include a watermark throughout the video.
While this process is straightforward, it takes a
significant amount of time and manual effort.

Professionals have developed both tools and design patterns
to speed up the process for video editing. They frequently
develop macros@note{In the context of video editing, a macro
 is a series of buttons that are automatically pressed in
 sequence.} for the tools they use. Most professional
tools even include an API to create new plugins.

Unfortunately, existing means of extending these tools are
insufficient. Macros are extremely brittle, and frequently
break. Using a tool's official plug-in interface is more
robust, but is still tied specifically to that tool.
Blender@cite[essential-blender] for example, is only
scriptable with a modified Python interpreter that runs when
Blender is launched.

A professional might then turn to a more generic framework,
only to find that they introduce a new set of problems.
These frameworks are frequently written for imperative C
like languages. Professional video editors are forced to
focus on managing the language rather than the actual
product. A professional may follow a design pattern to spend
less time managing the language, but then they are spending
time simply following the pattern. This process becomes
analogous to an author trying to write a paper using Java.
Even if they had access to nice string literals, they would
still spend a large portion of their time managing the java
side of their work. Thus, these frameworks are not
frequently used for interactive video editing, and are
generally used as a library for other video editors.

Existing XML style languages for video editing are better
suited for interactive video editing, but have their own
problems. Using XML directly removes abstraction, the very
reason why the professional left a traditional non-linear
video editor. They can regain abstraction by using the XML
processing capabilities of another programming language.
However, professionals that do this are back to the problem
of managing a generic programming language. Worse still, the
XML facilities in most programming languages are designed to
process generic XML files. So any developer wanting to take
this route will inevitably need to build up their own
standard library for useful XML based compositors.
Maintaining and extending this library becomes non-trivial,
even assuming that the professional knew about and followed
good software practices. Remember that while they are
spending all of this effort making abstractions, they are
not focusing on their actual work, editing videos. For this
reason, XML-style languages for video editing are frequently
used only by enthusiasts and tinkerers, who are more
interested in the world of video editing, rather than
editing their own videos.

A functional domain-specific language for editing videos reduces the repetition
in video editing, while not adding the overhead of a
managing a traditional programming language. Rather than
managing programs that process video, professionals can
describe the video itself, and let the language handle the
rest. Video editing professionals can work in a similar
fashion to authors typing prose in @exact{\LaTeX} or
Scribble@cite[scribble-icfp].

Video is a Racket based DSL for video editing. Users can
express their videos without having to handle all of the
boilerplate of setting up and maintaining video object and
rendering code. They simply describe the video they wish to
produce. Additionally, if a tasks become repetitive, they
have access to Racket's abstraction facilities. This
technique proves useful for editing many videos that have
similar templates, but are not always identical, such as
conference recordings or screencasts.

Video is also able to leverage the Racket programming
environment DrRacket@cite[plt-tr2] and graphical
toolkits@cite[plt-tr3]. Broadly speaking, video editors
benefit from what you see is what you get (WYSIWYG)
environments. DrRacket enables Video to allow to enable
WYSIWYG video editing when it is appropriate, and textual
editing when beneficial. Racket also contains an existing
API for creating pictures@cite[slideshow-jfp] and
sounds@note{@url["https://docs.racket-lang.org/rsound/index.html"]}
that Video can leverage.

This paper describes the design and implementation of Video,
a Racket based DSL for video editing. We introduce the
design of Video in @secref["overview"]. Next,
@secref["implementation"] discusses how Video is
implemented. In @secref["extensions"] we discuss graphical
extensions to video to aid in editing, and how it fits into
DrRacket's work flow. We compare Video to related work in
@secref["related"] and finally conclude in
@secref["conclusion"].
